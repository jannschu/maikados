%%% This file is part of Maikados.
%%% 
%%% Maikados is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%% 
%%% Maikados is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% 
%%% You should have received a copy of the GNU General Public License
%%% along with Maikados.  If not, see <http://www.gnu.org/licenses/>.
-module(maikados_players).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/0, add_player/1, set_player_name/2, challenge_player/2, accept_challenge/2]).

-include_lib("stdlib/include/ms_transform.hrl").

-include("protocol.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_player(Socket) ->
    gen_server:call(?MODULE, {add_player, Socket}).

set_player_name(Name, Pid) ->
    gen_server:call(?MODULE, {set_player_name, Name, Pid}).

challenge_player(From, To) ->
    gen_server:cast(?MODULE, {challenge_player, From, To}).

accept_challenge(From, Challenger) ->
    gen_server:cast(?MODULE, {accept_challenge, From, Challenger}).

%%% ======================================
%%%     gen_server CALLBACKS
%%% ======================================

-record(state, {}).
-record(player, {pid, name = undefined, game = undefined, challenges = gb_sets:new()}).
-record(game, {pid, playerA, playerB}).

init([]) ->
    process_flag(trap_exit, true),
    link(maikados_client_listener:setup()),
    ets:new(players, [named_table, {keypos, #player.pid}]),
    ets:new(games, [named_table, {keypos, #game.pid}]),
    {ok, #state{}}.

handle_call({add_player, Socket}, _From, #state{} = State) ->
    {ok, PlayerPid} = maikados_player:start_link(Socket),
    ets:insert(players, #player{pid = PlayerPid}),
    {reply, PlayerPid, State};

handle_call({set_player_name, Name, PlayerPid}, _From, #state{} = State) ->
    % whats the status of the player?
    Reply = case ets:lookup(players, PlayerPid) of
        [] -> error;
        [#player{name = undefined} = Player] ->
            % does name already exist?
            case find_player_by_name(Name) of
                not_found ->
                    broadcast_player_joined_lobby(Name),
                    send_lobby_members_to_player(PlayerPid),
                    ets:insert(players, Player#player{name = Name}),
                    ok;
                _Any ->
                    error
            end;
        [_Any] -> error % already name set
    end,
    {reply, Reply, State}.

handle_cast({challenge_player, FromName, ToName}, #state{} = State) ->
    case {find_player_by_name(FromName), find_player_by_name(ToName)} of
        {FromPlayer, ToPlayer} when FromPlayer =:= not_found; ToPlayer =:= not_found ->
            ok;
        {#player{pid = FromPid, challenges = FromsChallengesList} = From,
         #player{pid = ToPid, challenges = TosChallengesList} = To} ->
            % does To already challenge From?
            case gb_sets:is_member(FromPid, TosChallengesList) of
                true ->
                    start_game(From, To);
                false ->
                    NewList = gb_sets:add_element(ToPid, FromsChallengesList),
                    ets:insert(players, From#player{challenges = NewList}),
                    maikados_player:send_client_msg(ToPid, #lobby_challenge_player_msg{name = FromName})
            end
    end,
    {noreply, State};

handle_cast({accept_challenge, FromName, ChallengerName}, #state{} = State) ->
    case {find_player_by_name(FromName), find_player_by_name(ChallengerName)} of
        {FromPlayer, ChallPlayer} when FromPlayer =:= not_found; ChallPlayer =:= not_found ->
            ok;
        {#player{pid = FromPid} = FromPlayer,
         #player{challenges = ChallChallengesList} = ChallPlayer} ->
            case gb_sets:is_member(FromPid, ChallChallengesList) of
                false -> ok;
                true -> start_game(FromPlayer, ChallPlayer)
            end
    end,
    {noreply, State}.

-define(IF(Cond, Body), if Cond -> Body; true -> ok end).

handle_info({'EXIT', From, _Reason}, #state{} = State) ->
    case ets:lookup(players, From) of
        [#player{name = Name, game = GamePid}] ->
            ets:delete(players, From),
            if
                GamePid =:= undefined -> broadcast_player_left_lobby(Name);
                true -> ok % will be handled when game process crashes
            end;
        [] ->
            case ets:lookup(games, From) of
                [] -> ok;
                [#game{playerA = PidA, playerB = PidB}] ->
                    ets:delete(games, From),
                    % doing it with dead players does not hurt xD
                    maikados_player:return_to_lobby(PidA),
                    maikados_player:return_to_lobby(PidB),
                    send_lobby_members_to_player(PidA),
                    send_lobby_members_to_player(PidB),
                    ets:update_element(players, PidA, {#player.game, undefined}),
                    ets:update_element(players, PidB, {#player.game, undefined}),
                    case ets:lookup(players, PidA) of
                        [#player{name = NameA}] -> broadcast_player_joined_lobby(NameA);
                        [] -> ok
                    end,
                    case ets:lookup(players, PidB) of
                        [#player{name = NameB}] -> broadcast_player_joined_lobby(NameB);
                        [] -> ok
                    end
            end
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    maikados_client_listener:setdown(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ======================================
%%%     HELPERS
%%% ======================================

start_game(#player{pid = PidA, name = NameA} = A, #player{pid = PidB, name = NameB} = B) ->
    {ok, GamePid} = maikados_game:start_link({PidA, NameA}, {PidB, NameB}),
    ets:insert(games, #game{pid = GamePid, playerA = PidA, playerB = PidB}),
    ets:insert(players, A#player{game = GamePid, challenges = gb_sets:new()}),
    ets:insert(players, B#player{game = GamePid, challenges = gb_sets:new()}),
    broadcast_player_left_lobby(NameA),
    broadcast_player_left_lobby(NameB).

broadcast_player_left_lobby(Name) ->
    send_msg_to_all_players(#lobby_player_left_msg{name = Name}, Name).

broadcast_player_joined_lobby(Name) ->
    send_msg_to_all_players(#lobby_set_player_msg{list = [Name]}, Name).

send_lobby_members_to_player(PlayerPid) -> send_lobby_members_to_player(PlayerPid, []).
send_lobby_members_to_player(PlayerPid, Blacklist) ->
    MS = ets:fun2ms(fun(#player{pid = P, game = undefined, name = N, _ = '_'})
            when N =/= undefined, P =/= PlayerPid -> N end),
    Names = ets:select(players, MS),
    Message = #lobby_set_player_msg{list = lists:filter(fun(P) -> not lists:member(P, Blacklist) end, Names)},
    maikados_player:send_client_msg(PlayerPid, Message).

find_player_by_name(Name) ->
    case ets:match_object(players, #player{name = Name, _ = '_'}) of
        [#player{} = Player] -> Player;
        [] -> not_found
    end.

send_msg_to_all_players(Message, BlackListName) ->
    MS = ets:fun2ms(fun(#player{pid = P, game = undefined, name = N, _ = '_'})
            when N =/= undefined, N =/= BlackListName -> P end),
    PlayerPids = ets:select(players, MS),
    [maikados_player:send_client_msg(Pid, Message) || Pid <- PlayerPids].