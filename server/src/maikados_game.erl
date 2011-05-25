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
-module(maikados_game).

-behaviour(gen_fsm).
-export([init/1, handle_event/3, code_change/4, handle_info/3,
    handle_sync_event/4, terminate/3,
    wait_for_game_action/2]).

-export([start_link/2, msg/3, player_left/2, stop/1]).

-include("protocol.hrl").

%% --------------------------------------
%% @doc starts gen_fsm
%% @end
%% --------------------------------------
start_link(Player0, Player1) ->
    gen_fsm:start_link(?MODULE, [Player0, Player1], []).

%% --------------------------------------
%% @doc stops fsm
%% @end
%% --------------------------------------
stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

player_left(Pid, Name) ->
    gen_fsm:send_all_state_event(Pid, {player_left, Name}).

msg(Pid, Side, Msg) ->
    gen_fsm:send_event(Pid, {Side, Msg}).

%%% ======================================
%%%     gen_fsm
%%% ======================================

-record(state, {pid0, name0, pid1, name1, piece = chose, player = 0, blockedMoves = 0, field}).

init([{Pid0, Player0}, {Pid1, Player1}]) ->
    Field = maikados_field:create(),
    InitPieces = maikados_field:get_field_msg(Field),
    maikados_player:send_client_msg(Pid0, #srv_game_start_msg{opponent = Player1, side = 0, pieces = InitPieces}),
    maikados_player:send_client_msg(Pid0, #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_ChoosePiece, data = ?MOVE_TIME / 2}),
    
    maikados_player:send_client_msg(Pid1, #srv_game_start_msg{opponent = Player0, side = 1, pieces = InitPieces}),
    maikados_player:send_client_msg(Pid1, #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_WaitForOpponent, data = [?MOVE_TIME / 2, null]}),
    
    maikados_player:receive_msg(Pid0, {game_start, 0, self()}),
    maikados_player:receive_msg(Pid1, {game_start, 1, self()}),
    
    {ok, wait_for_game_action, #state{
        pid0 = Pid0, name0 = Player0,
        pid1 = Pid1, name1 = Player1,
        field = Field}}.

wait_for_game_action({Player, #game_action_msg{action = ?GAME_ACTION_MSG_PieceChosen, data = Chose}}, #state{piece = chose, player = Player, field = Field} = State) ->
    PiecePair = {Player, Chose},
    NewState = State#state{piece = PiecePair},
    PieceID = piece_id(PiecePair),
    Fields = maikados_field:get_moves_for(Field, PiecePair),
    ChoseFieldMsg = #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_ChooseField, data = [PieceID, Fields, ?MOVE_TIME]},
    maikados_player:send_client_msg(get_pid_for_player(Player, State), ChoseFieldMsg),
    maikados_player:send_client_msg(get_pid_for_player(1 - Player, State), #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_WaitForOpponent, data = [?MOVE_TIME, PieceID]}),
    {next_state, wait_for_game_action, NewState};

wait_for_game_action(
        {Player, #game_action_msg{action = ?GAME_ACTION_MSG_FieldChosen, data = Field} = Msg},
        #state{piece = Piece, player = Player, field = GameField} = State) when Piece =/= chose ->
    maikados_player:send_client_msg(get_pid_for_player(1 - Player, State), Msg),
    State2 = if
        Field =/= null ->
            GameField1 = maikados_field:move_piece(GameField, Piece, Field),
            S = case (Field div 8) rem 7 of % add dragon Tooth?
                0 ->
                    State#state{field = maikados_field:add_dragon_tooth(GameField1, Piece)};
                _ ->
                    State#state{field = GameField1}
            end,
            S#state{blockedMoves = 0};
        true ->
            State#state{blockedMoves = State#state.blockedMoves + 1}
    end,
    Points = maikados_field:get_points_for_player(State2#state.field, Player),
    WinningPoints = 1, % TODO: mode dependent value here
    if
        Points >= WinningPoints orelse State2#state.blockedMoves =:= 2 ->
            {A, B} = if
                State2#state.blockedMoves =:= 2 -> {1 - Player, Player};
                true -> {Player, 1 - Player}
            end,
            maikados_player:send_client_msg(get_pid_for_player(A, State), #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_YouWin}),
            maikados_player:send_client_msg(get_pid_for_player(B, State), #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_YouLost}),
            {stop, normal, State2};
        true ->
            NextColor = if
                Field =:= null -> maikados_field:get_color_id_for_field(State#state.field, Piece);
                true -> maikados_field:get_color_id_for_field(Field)
            end,
            NextPiece = {1 - Player, NextColor},
            Fields = maikados_field:get_moves_for(State2#state.field, NextPiece),
            State3 = State2#state{player = 1 - Player, piece = NextPiece},
            PieceID = piece_id(NextPiece),
            ChoseFieldMsg = #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_ChooseField, data = [PieceID, Fields, ?MOVE_TIME]},
            maikados_player:send_client_msg(get_pid_for_player(1 - Player, State), ChoseFieldMsg),
            maikados_player:send_client_msg(get_pid_for_player(Player, State), #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_WaitForOpponent, data = [?MOVE_TIME, PieceID]}),
            {next_state, wait_for_game_action, State3}
    end;

wait_for_game_action(Any, State) ->
    error_logger:warning_msg("Unexpected msg in game: ~p", [Any]),
    {next_state, wait_for_game_action, State}.

handle_event({player_left, _Name}, _StateName, State) ->
    {stop, player_left, State};

handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(normal, _StateName, _StateData) -> ok;
terminate(shutdown, _StateName, _StateData) -> ok;

terminate(_Reason, _StateName, State) ->
    Msg = #srv_game_ctrl_msg{code = ?SRV_GAME_CTRL_MSG_LostOpponentConnection},
    maikados_player:send_client_msg(State#state.pid0, Msg),
    maikados_player:send_client_msg(State#state.pid1, Msg).

get_pid_for_player(0, #state{pid0 = P}) -> P;
get_pid_for_player(1, #state{pid1 = P}) -> P.

piece_id({Player, Color}) ->
    list_to_binary([48 + Player | ("-" ++ integer_to_list(Color))]).
