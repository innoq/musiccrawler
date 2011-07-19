%% Copyright 2011 Ulf Angermann, Martin Huber
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
%%% Author  : Martin Huber (martin.huber@innoq.com)
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(mc_controller).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/mc.hrl"). 
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, start_link/0, start_child/2, terminate/2, start_search/0]).

-define(SERVER, ?MODULE).


%% ====================================================================
%% External functions
%% ====================================================================

terminate(X,Y) ->
	io:format("Terminate:~p~p~n", [X,Y]).

%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Rstation, File ) ->
	supervisor:start_child(?SERVER, [Rstation, File]).

init([]) -> 
	IcyServer = {mc_icy, {mc_icy, start_link, []}, permanent, brutal_kill, worker, [mc_icy]},
	Children = [IcyServer],
	RestartStrategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.
	

 %% ===================================================================
%% API
%% ===================================================================

start_search() ->
	L=mc_config:stations(),
	start_search(L, "/Users/martinh/Music/musiccrawler/").


%% ===================================================================
%% Internal Functions
%% ===================================================================

start_search([], _) -> ok;
start_search([Station|T], File) ->
	mc_controller:start_child(Station, File),
	start_search(T, File).
 