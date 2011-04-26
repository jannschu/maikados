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

-record(player, {pid}).

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
    {ok, dict:new()}.

handle_call({register, Name, Pid}, _From, Dict) ->
    {Reply, NewDict} = case dict:is_key(Name, Dict) of
        true ->
            {error, Dict};
        false ->
            monitor(process, Pid),
            {ok, dict:store(Name, #player{pid = Pid}, Dict)}
    end,
    {reply, Reply, NewDict}.

handle_cast({player_left, Name}, Dict) ->
    {noreply, dict:erase(Name, Dict)};

handle_cast(_Request, Dict) ->
    {noreply, Dict}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, Pid}, Dict) ->
    Names = dict:fetch_keys(dict:filter(fun(_Key, Val) ->
        Val#player.pid =:= Pid
    end, Dict)),
    NewDict = lists:foldl(fun(Name, S) -> dict:erase(Name, S) end, Dict, Names),
    {noreply, NewDict};

handle_info(_Info, Dict) ->
    {noreply, Dict}.

terminate(_Reason, _Dict) ->
    ok.

code_change(_OldVsn, Dict, _Extra) ->
    {ok, Dict}.