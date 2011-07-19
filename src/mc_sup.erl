%% Copyright 2011 Ulf Angermann / Martin Huber
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Martin Huber / martin.huber@innoq.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(mc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

	ConfigServer = {mc_config, {mc_config, start, []},  permanent, 2000, worker, [mc_config]},
	Controller = {mc_controller, {mc_controller, start_link, []},  permanent, 2000, supervisor, [mc_controller]}, 
	Children = [Controller, ConfigServer],
	RestartStrategy = {one_for_one, 4, 3600},
	{ok, {RestartStrategy, Children}}.

