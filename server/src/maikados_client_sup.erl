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
%% @doc Supervisor for all connected clients
%% @end
%% --------------------------------------
-module(maikados_client_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).

-export([new_client/1]).

%% --------------------------------------
%% @doc Creates new maikados_client and returns Pid
%% @end
%% --------------------------------------
new_client(Args) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Args]),
    Pid.

%% --------------------------------------
%% @doc supervisor behaviour callback
%% @private
%% --------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------
%% @doc supervisor behaviour callback
%% @private
%% --------------------------------------
init(_Args) ->
    Spec = {maikados_client, {maikados_client, start_link, []},
            temporary, 5000, worker, [maikados_client]},
    {ok, {{simple_one_for_one, 5, 60}, [Spec]}}.
