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
    login/2, wait_for_game_start/2, forward_messages/2]).

-export([start_link/1, receive_msg/2, stop/1, send_client_msg/2]).

-include("protocol.hrl").

-record(client, {socket, name, game, side}).

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

%% --------------------------------------
%% @doc Send message to client (via socket)
%% @end
%% --------------------------------------
send_client_msg(Pid, Msg) ->
    gen_fsm:send_all_state_event(Pid, {send, Msg}).

%%% ======================================
%%%     gen_fsm
%%% ======================================

init(SocketPid) ->
    {ok, login, #client{socket = SocketPid}}.

login(#clt_login{name = Name}, State) ->
    error_logger:info_msg("Login try by player ~p~n", [Name]),
    {NextState, NewState} = case is_valid_nick(Name) of
        true ->
            case maikados_players:register_player(Name, self()) of
                error ->
                    send_msg(State, #response_code_msg{code = ?RESPONSE_CODE_MSG_Illegal}),
                    {login, State};
                ok ->
                    send_msg(State, #response_code_msg{code = ?RESPONSE_CODE_MSG_OK}),
                    {wait_for_game_start, State#client{name = Name}}
            end;
        false ->
            send_msg(State, #response_code_msg{code = ?RESPONSE_CODE_MSG_Illegal}),
            {login, State}
    end,
    {next_state, NextState, NewState};

login(Msg, State) ->
    error_logger:info_msg("Got unhandled message in state `login': ~p~n", [Msg]),
    {next_state, login, State}.

wait_for_game_start({game_start, Side, Game}, State) ->
    {next_state, forward_messages, State#client{game = Game, side = Side}};

wait_for_game_start(Msg, State) ->
    error_logger:info_msg("Got unhandled message in state `wait_for_game_start': ~p~n", [Msg]),
    {next_state, wait_for_game_start, State}.

forward_messages(Msg, #client{game = G, side = S} = State) ->
    maikados_game:msg(G, S, Msg),
    {next_state, forward_messages, State}.

handle_event({send, Msg}, StateName, State) ->
    send_msg(State, Msg),
    {next_state, StateName, State};

handle_event(stop, _StateName, State) ->
    maikados_players:player_left(State#client.name),
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

send_msg(#client{socket = Pid}, Msg) -> send_msg(Pid, Msg, is_tuple(Msg)).

send_msg(Pid, Msg, Json) ->
    {ok, Content} = maikados_protocol:record_to_packet(Msg),
    socketio_client:send(Pid, #msg{ content = Content, json = Json }).

is_valid_nick(Nick) ->
    % TODO: fix char count
    {ok, Pattern} = re:compile("^[- %a-z0-9_öäüß@.]{1,30}$", [caseless]),
    re:run(Nick, Pattern, [{capture, none}]) =:= match.