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
-module(mc_config).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-define(STATION_CONFIG, filename:join([code:priv_dir("musiccrawler"), "rstations.config"])).
-define(MUSICLIST_CONFIG, filename:join([code:priv_dir("musiccrawler"), "musiclist.config"])).
-define(GENRE_CONFIG, filename:join([code:priv_dir("musiccrawler"), "genres.config"])).
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(	state, 
					{genres, 
					 stations, 
					 musiclist}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	{ok, [Stations]} = file:consult(?STATION_CONFIG),
	{ok, [MusicL]} = file:consult(?MUSICLIST_CONFIG),
	{ok, [ValidGenres]} = file:consult(?GENRE_CONFIG),
	true = check_stations(Stations, ValidGenres),
    {ok, #state{genres=ValidGenres, stations=Stations, musiclist=MusicL}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Args:	{stations} -> lists all configured stations as record-list
%% Description: lists all configured stations as record-list
%% Returns: {reply, Reply, State}          |
%% --------------------------------------------------------------------
handle_call({stations}, From, State) ->
    Reply = State#state.stations,
    {reply, Reply, State};
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Args:	{stations, [Genres]} ->  
%% Description: Handling call messages, lists all configured stations, that are in the given genre-types-list
%% Returns: {reply, Reply, State}          |
%% --------------------------------------------------------------------

handle_call({stations, Genre}, From, State) ->
    Reply = get_stations_by_genre(State#state.stations, Genre),
    {reply, Reply, State}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

check_stations(Stations, Genres) ->	
	BoolList=[lists:member(K, Genres) || {_,K,_,_,_}<-Stations],
	not lists:member(false, BoolList).
	
get_stations_by_genre(Stations, Genres) ->
	[K || K = {_,X2,_,_,_} <- Stations, lists:member(X2, Genres)].

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------

startstop_test() -> 
	start(),
	stop().
	

check_stations_test() ->
	Stations= [{x, genre1, x, x, x }, {x, genre2, x, x, x }, {x, genre1, x, x, x } ],
	StationsWrong= [{x, genre31, x, x, x }, {x, genre2, x, x, x}, {x, genre1, x, x, x }],
	Genres = [genre1, genre2],
	true = check_stations(Stations, Genres),
	false = check_stations(StationsWrong, Genres),
	ok.
	
get_stations_bygenre_test() ->
	Stations= [{x, genre1, x, x, x }, {x, genre2, x, x, x }, {x, genre1, x, x, x }, {x, genre1, x, x, x }, {x, genr31, x, x, x } ],
	Genres = [genre1],									% Test with only one genre
	L = get_stations_by_genre(Stations, Genres),
	true = check_stations(L, Genres),					% All found of Correct genre
	Genres2 = [genre1, genre2],							% Test with multiple genres
	L2 = get_stations_by_genre(Stations, Genres),
	true = check_stations(L2, Genres2),					% All found of Correct genre
	[] = get_stations_by_genre(Stations, [wronggenre]),   % Empty when a wrong genre is given
	
	ok.
