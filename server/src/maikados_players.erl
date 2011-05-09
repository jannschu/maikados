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

-export([start_link/0, register_player/2, player_left/1]).

-record(player, {pid, game = null}).
-record(state, {players = dict:new(), waiting_player = null}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_player(Name, Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

player_left(Name) ->
    gen_server:cast(?MODULE, {player_left, Name}).

%%% ======================================
%%%     gen_server CALLBACKS
%%% ======================================

init([]) ->
    {ok, #state{}}.

handle_call({register, Name, Pid}, _From, #state{players = Dict, waiting_player = WaitingPlayer} = State) ->
    {Reply, NewDict, NewWaitingPlayer} = case dict:is_key(Name, Dict) of
        true ->
            {error, Dict, WaitingPlayer};
        false ->
            monitor(process, Pid),
            DictOrg = dict:store(Name, #player{pid = Pid}, Dict),
            {WaitingPlayer2, Dict2} = case WaitingPlayer of
                null ->
                    {Name, DictOrg};
                _ ->
                    GamePid = maikados_game_sup:new_game({(dict:fetch(WaitingPlayer, DictOrg))#player.pid, WaitingPlayer}, {Pid, Name}),
                    Update = fun(N, D) ->
                        dict:update(N, fun(P) -> P#player{game = GamePid} end, D)
                    end,
                    Dict0 = Update(WaitingPlayer, DictOrg),
                    Dict1 = Update(Name, Dict0),
                    {null, Dict1}
            end,
            {ok, Dict2, WaitingPlayer2}
    end,
    {reply, Reply, State#state{players = NewDict, waiting_player = NewWaitingPlayer}}.

handle_cast({player_left, Name}, #state{players = Dict, waiting_player = WaitingPlayer} = State) ->
    NewWaitingPlayer = case WaitingPlayer of
        Name -> null;
        _ -> WaitingPlayer
    end,
    case dict:find(Name, Dict) of
        {ok, #player{game = Game}} when is_pid(Game) ->
            maikados_game:player_left(Game, Name);
        _ -> ok
    end,
    {noreply, State#state{players = dict:erase(Name, Dict), waiting_player = NewWaitingPlayer}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, Pid}, #state{players = Dict} = State) ->
    Names = dict:fetch_keys(dict:filter(fun(_Key, Val) ->
        Val#player.pid =:= Pid
    end, Dict)),
    NewDict = lists:foldl(fun(Name, S) -> dict:erase(Name, S) end, Dict, Names),
    {noreply, State#state{players = NewDict}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.