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
%% @doc Listener process for incoming connections
%% @end
%% --------------------------------------
-module(maikados_client_listener).

-include_lib("deps/socketio/include/socketio.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2,
    handle_call/2, handle_info/2, code_change/3, terminate/2]).

-export([start_link/0, handle_request/3]).

%% --------------------------------------
%% @doc Starts to listen, registers event callback
%% @end
%% --------------------------------------
start_link() ->
    Pid = try socketio_listener:start([{http_port, 7878},
                                         {default_http_handler, ?MODULE}]) of
        {ok, P} -> P
    catch
        error -> socketio_listener:server(socketio_listener_sup)
    end,
    EventMgr = socketio_listener:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE, []),
    {ok, spawn_link(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
            {'EXIT', Pid, _Reason} ->
                ok
        end
    end)}.

%%% ======================================
%%% gen_event CALLBACKS
%%% ======================================

init([]) ->
    {ok, dict:new()};

init([Pid]) ->
    {ok, Pid}.

handle_event({client, Client}, State) ->
    error_logger:info_msg("New client connected~n"),
    ClientServer = maikados_client_sup:new_client(Client),
    NewState = dict:store(Client, ClientServer, State),
    EventMgr = socketio_client:event_manager(Client),
    ok = gen_event:add_handler(EventMgr, ?MODULE, [ClientServer]),
    {ok, NewState};

handle_event({message, _Client, #msg{ content = Content }}, Pid) ->
    maikados_client:receive_msg(Pid, Content),
    {ok, Pid};

handle_event({disconnect, Client}, State) ->
    error_logger:info_msg("Client disconnected~n"),
    case dict:find(Client, State) of
        {ok, ClientPid} ->
            maikados_client:stop(ClientPid);
        error ->
            ok
    end,
    {ok, dict:erase(Client, State)};

handle_event(Event, State) ->
    io:format("EVENT: ~p~n", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    Reply = Request,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    todo.

%%% ======================================
%%% http handler
%%% ======================================

%% --------------------------------------
%% @doc http request callback function
%% @see start()
%% @private
%% --------------------------------------
handle_request(_Method, Path, Req) ->
    case is_valid_path(Path) of
        {true, File} ->
            FilePath = "../" ++ File,
            case filelib:is_file(FilePath) of
                true ->
                    Req:file(FilePath);
                false ->
                    Req:respond(404)
            end;
        false ->
            error_logger:info_msg("Forbidden file requested: ~p~n", [Path]),
            Req:respond(403)
    end.

is_valid_path([]) -> {true, "index.html"};
is_valid_path(["lib"|_Rest] = Path) -> {true, filename:join(Path)};
is_valid_path(["resources"|_Rest] = Path) -> {true, filename:join(Path)};
is_valid_path(["index.html" = Path]) -> {true, Path};
is_valid_path(["favicon.ico" = Path]) -> {true, Path};

is_valid_path(_Any) -> false.
