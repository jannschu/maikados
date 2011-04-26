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

-record(clt_login, {name}).
-record(srv_response_code, {code}).

-ifdef(DEFINE_PROTCOL_COMMAND_FUNCTIONS).

cmd2nr(clt_login) -> {ok, 0};
cmd2nr(srv_response_code) -> {ok, 1};
cmd2nr(_) -> error.

nr2cmd(0) -> {ok, clt_login};
nr2cmd(1) -> {ok, srv_response_code};
nr2cmd(_) -> error.

cmd_fields(clt_login) -> {ok, [name]};
cmd_fields(srv_response_code) -> {ok, [code]};
cmd_fields(_) -> error.

-endif.
