-module(maikados_player_events).
-behaviour(gen_event).
-export([init/1, handle_event/2,
    handle_call/2, handle_info/2, code_change/3, terminate/2]).

%%% ======================================
%%% gen_event CALLBACKS
%%% ======================================

-include_lib("deps/socketio/include/socketio.hrl").

init(Pid) ->
    {ok, Pid}.

handle_event({message, _Client, #msg{ content = Content, json = Json }}, Pid) when Json =:= true ->
    case maikados_protocol:packet_to_record(Content) of
        {ok, Record} ->
            maikados_player:receive_msg(Pid, Record);
        error ->
            error_logger:info_msg("Unknown packet received: ~p~n", [Content])
    end,
    {ok, Pid};

handle_event({message, _Client, #msg{ content = Content}}, Pid) ->
    error_logger:warning_msg("Plain message received: ~p~n", [Content]),
    {ok, Pid};

handle_event(Event, State) ->
    error_logger:info_msg("unhandled EVENT in player_events: ~p~n", [Event]),
    {ok, State}.

handle_call(Request, State) ->
    Reply = Request,
    {ok, Reply, State}.

handle_info(_Request, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.