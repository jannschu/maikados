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

-export([start_link/0, add_player/1, set_player_name/2]).

-record(state, {players = dict:new(), in_game_players = gb_sets:new(), games = dict:new()}).

-include("protocol.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_player(Socket) ->
    gen_server:call(?MODULE, {add_player, Socket}).

set_player_name(Name, Pid) ->
    gen_server:call(?MODULE, {set_player_name, Name, Pid}).

%%% ======================================
%%%     gen_server CALLBACKS
%%% ======================================

init([]) ->
    process_flag(trap_exit, true),
    link(maikados_client_listener:setup()),
    {ok, #state{}}.

handle_call({add_player, Socket}, _From, #state{players = Players} = State) ->
    {ok, PlayerPid} = maikados_player:start_link(Socket),
    NewPlayers = dict:store(PlayerPid, undefined, Players),
    {reply, PlayerPid, State#state{players = NewPlayers}};

handle_call({set_player_name, Name, PlayerPid}, _From, #state{players = Players} = State) ->
    {Reply, NewPlayers} = case dict:find(PlayerPid, Players) of
        error -> {error, Players};
        {ok, undefined} ->
            case dict:to_list(dict:filter(fun(_Pid, NameDict) -> NameDict =:= Name end, Players)) of
                [] ->
                    InGame = State#state.in_game_players,
                    OtherPlayerPids = [Pid || {Pid, OppName} <- dict:to_list(Players),
                        not gb_sets:is_member(Pid, InGame), OppName =/= undefined],
                    broadcast_player_joined_lobby(Name, OtherPlayerPids),
                    OtherPlayerNames = [dict:fetch(Pid, Players) || Pid <- OtherPlayerPids],
                    send_lobby_memebers_to_player(PlayerPid, OtherPlayerNames),
                    {ok, dict:store(PlayerPid, Name, Players)};
                _Any ->
                    {error, Players}
            end;
        {ok, _Any} -> {error, Players} % already name set
    end,
    {reply, Reply, State#state{players = NewPlayers}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', From, _Reason}, #state{players = Players} = State) ->
    % TODO: implement for games, send del msg for lobby
    case dict:is_key(From, Players) of
        true ->
            {noreply, State#state{players = dict:erase(From, Players)}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ======================================
%%%     HELPERS
%%% ======================================

broadcast_player_joined_lobby(Name, PlayerPids) ->
    Message = #lobby_set_player_msg{list = [Name]},
    [maikados_player:send_client_msg(Pid, Message) || Pid <- PlayerPids].

send_lobby_memebers_to_player(_PlayerPid, []) -> ok;
send_lobby_memebers_to_player(PlayerPid, Names) ->
    Message = #lobby_set_player_msg{list = Names},
    maikados_player:send_client_msg(PlayerPid, Message).