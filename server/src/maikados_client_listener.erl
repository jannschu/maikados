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

-include("protocol.hrl").

-behaviour(gen_event).
-export([init/1, handle_event/2,
    handle_call/2, handle_info/2, code_change/3, terminate/2]).

-export([setup/0, handle_request/3]).

%% --------------------------------------
%% @doc Starts to listen, registers event callback
%% @end
%% --------------------------------------
setup() ->
    Pid = try
        socketio_listener:server(socketio_listener_sup)
    catch
        _:_ ->
            {ok, P} = socketio_listener:start([{http_port, 7878},
                                             {default_http_handler, ?MODULE}]),
            P
    end,
    EventMgr = socketio_listener:event_manager(Pid),
    case lists:any(fun(H) -> H =:= ?MODULE end, gen_event:which_handlers(EventMgr)) of
        false ->
            ok = gen_event:add_handler(EventMgr, ?MODULE, []);
        _ ->
            ok
    end,
    EventMgr.

%%% ======================================
%%% gen_event CALLBACKS
%%% ======================================

-record(state, {players = dict:new()}).

init([]) ->
    {ok, #state{}}.

handle_event({client, Client}, #state{players = PlayerList} = State) ->
    error_logger:info_msg("New client connected: ~p", [Client]),
    PlayerPid = maikados_players:add_player(Client),
    NewState = State#state{players = dict:store(Client, PlayerPid, PlayerList)},
    {ok, NewState};

handle_event({disconnect, Client}, #state{players = Players} = State) ->
    error_logger:info_msg("Client disconnected: ~p", [Client]),
    maikados_player:stop(dict:fetch(Client, Players)),
    {ok, State#state{players = dict:erase(Client, State#state.players)}};

handle_event(Event, State) ->
    error_logger:info_msg("unhandled EVENT in client_listener: ~p", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    Reply = Request,
    {ok, Reply, State}.

handle_info(_Request, #state{} = State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

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
            error_logger:info_msg("Forbidden file requested: ~p", [Path]),
            Req:respond(403)
    end.

is_valid_path([]) -> {true, "index.html"};
is_valid_path(["lib"|_Rest] = Path) -> {true, filename:join(Path)};
is_valid_path(["resources"|_Rest] = Path) -> {true, filename:join(Path)};
is_valid_path(["index.html" = Path]) -> {true, Path};
is_valid_path(["favicon.ico" = Path]) -> {true, Path};

is_valid_path(_Any) -> false.
