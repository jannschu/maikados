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

%% --------------------------------------
%% @doc Client server
%% @end
%% --------------------------------------
-module(maikados_client).

-include_lib("deps/socketio/include/socketio.hrl").

-behaviour(gen_fsm).
-export([init/1, handle_event/3, code_change/4, handle_info/3,
    handle_sync_event/4, terminate/3,
    login/2]).

-export([start_link/1, receive_msg/2, stop/1]).

-include("protocol.hrl").

-record(client, {socket}).

%% --------------------------------------
%% @doc starts gen_fsm
%% @end
%% --------------------------------------
start_link(SocketPid) ->
    gen_fsm:start_link(?MODULE, SocketPid, []).

%% --------------------------------------
%% @doc stops server
%% @end
%% --------------------------------------
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

%% --------------------------------------
%% @doc Sends client message to fsm
%% @end
%% --------------------------------------
receive_msg(Pid, Msg) ->
    gen_fsm:send_event(Pid, Msg).

%%% ======================================
%%%     gen_fsm
%%% ======================================

init(SocketPid) ->
    {ok, login, #client{socket = SocketPid}}.

login(Msg, State) ->
    error_logger:info_msg("Got message: ~p~n", [Msg]),
    send_msg(State, [{<<"msg">>, 15}]),
    {next_state, login, State}.

handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

%%% ======================================
%%%     helper
%%% ======================================

send_msg(#client{socket = Pid}, [_Foo] = Msg) -> send_msg(Pid, Msg, true);
send_msg(#client{socket = Pid}, Msg) -> send_msg(Pid, Msg, false).

send_msg(#client{socket = Pid}, Msg, Json) ->
    socketio_client:send(Pid, #msg{ content = Msg, json = Json }).