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

%%% ======================================
%%%     commands
%%% ======================================

-record(clt_login, {name}). % 0

-record(response_code_msg, {code}). % 1
-define(RESPONSE_CODE_MSG_OK, 0).
-define(RESPONSE_CODE_MSG_Illegal, 1).
-define(RESPONSE_CODE_MSG_Yes, 2).
-define(RESPONSE_CODE_MSG_No, 3).
-define(RESPONSE_CODE_MSG_Left, 4).
-define(RESPONSE_CODE_MSG_Right, 5).

-record(srv_game_start_msg, {opponent, side, pieces}). % 2

-record(srv_game_ctrl_msg, {code, data = null}). % 3
-define(SRV_GAME_CTRL_MSG_WaitForOpponent, 0).
-define(SRV_GAME_CTRL_MSG_LostOpponentConnection, 1).
-define(SRV_GAME_CTRL_MSG_ChoosePiece, 2).
-define(SRV_GAME_CTRL_MSG_ChooseField, 3). % data = [pieceID, fields, time]
-define(SRV_GAME_CTRL_MSG_YouLost, 4).
-define(SRV_GAME_CTRL_MSG_YouWin, 5).
-define(SRV_GAME_CTRL_MSG_NextModeQuestion, 6).
-define(SRV_GAME_CTRL_MSG_NextMode, 7).

-record(game_action_msg, {action, data = null}). % 4
-define(GAME_ACTION_MSG_PieceChosen, 0).
-define(GAME_ACTION_MSG_FieldChosen, 1).

-define(MOVE_TIME, 60). % in seconds

-ifdef(DEFINE_PROTCOL_COMMAND_FUNCTIONS).

cmd2nr(clt_login) -> {ok, 0};
cmd2nr(response_code_msg) -> {ok, 1};
cmd2nr(srv_game_start_msg) -> {ok, 2};
cmd2nr(srv_game_ctrl_msg) -> {ok, 3};
cmd2nr(game_action_msg) -> {ok, 4};
cmd2nr(_) -> error.

nr2cmd(0) -> {ok, clt_login};
nr2cmd(1) -> {ok, response_code_msg};
nr2cmd(2) -> {ok, srv_game_start_msg};
nr2cmd(3) -> {ok, srv_game_ctrl_msg};
nr2cmd(4) -> {ok, game_action_msg};
nr2cmd(_) -> error.

cmd_fields(clt_login) -> {ok, [name]};
cmd_fields(response_code_msg) -> {ok, [code]};
cmd_fields(srv_game_start_msg) -> {ok, [opponent, side, pieces]};
cmd_fields(srv_game_ctrl_msg) -> {ok, [code, data]};
cmd_fields(game_action_msg) -> {ok, [action, data]};
cmd_fields(_) -> error.

-endif.
