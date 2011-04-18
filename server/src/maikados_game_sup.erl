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
%% @doc Supervisor for all running games
%% @end
%% --------------------------------------
-module(maikados_game_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

%% --------------------------------------
%% @doc supervisor behaviour callback
%% @private
%% --------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%% --------------------------------------
%% @doc supervisor behaviour callback
%% @private
%% --------------------------------------
init(_Args) ->
    Spec = {maikados_game, {maikados_game, start_link, []},
            temporary, 15000, worker, [maikados_game]},
    {ok, {{simple_one_for_one, 5, 60}, [Spec]}}.
    