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

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_request/3]).

-export([start/0]).

%% --------------------------------------
%% @doc Starts to listen, registers event callback
%% @end
%% --------------------------------------
start() ->
    {ok, Pid} = socketio_listener:start([{http_port, 7878},
                                         {default_http_handler, ?MODULE}]),
    EventMgr = socketio_listener:event_manager(Pid),
    ok = gen_event:add_handler(EventMgr, ?MODULE, []).

%% --------------------------------------
%% @doc gen_event callback function
%% @private
%% --------------------------------------
init([]) ->
    State = dict:new(),
    {ok, State}.

%% --------------------------------------
%% @doc gen_event callback function
%% @private
%% --------------------------------------
handle_event(Event, State) ->
    io:format("EVENT: ~p~n", [Event]),
    {ok, State}.

%% --------------------------------------
%% @doc gen_event callback function
%% @private
%% --------------------------------------
terminate(_Reason, _State) ->
    todo.

%% --------------------------------------
%% @doc http request callback function
%% @see start()
%% @private
%% --------------------------------------
handle_request(_Method, _Path, Req) ->
    Path = check_for_index(case Req:get(uri_unquoted) of
        [$/|P] ->
            P;
        P -> P
    end),
    WhiteList = [
        "index.html",
        "lib/socket.io/socket.io.min.js",
        "resources/master.css",
        "lib/raphael/raphael-min.js",
        "resources/maikados.js",
        "resources/gplv3-88x31.png",
        "favicon.ico"
    ],
    case lists:any(fun(El) -> El =:= Path end, WhiteList) of
        true ->
            File = "../" ++ Path,
            case filelib:is_file(File) of
                true ->
                    Req:file(File);
                false ->
                    Req:respond(404)
            end;
        false ->
            Req:respond(403)
    end.

check_for_index("") -> "index.html";
check_for_index(Any) -> Any.