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
%% @doc Callback module for application behaviour
%% @end
%% --------------------------------------
-module(maikados_app).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([start_link/0, init/1]).

%% --------------------------------------
%% @doc application behaviour callback
%% @private
%% --------------------------------------
start(normal, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

%% --------------------------------------
%% @doc application behaviour callback
%% @private
%% --------------------------------------
stop(_State) ->
    ok.

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
    Children = [
        {maikados_players, {maikados_players, start_link, []},
         permanent, 5000, worker, [maikados_players]}
    ],
    {ok, {{one_for_one, 5, 60}, Children}}.
