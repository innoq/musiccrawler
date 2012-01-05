%%% -------------------------------------------------------------------
%% Copyright 2011 Martin Huber
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
-module(mc_db).

%%% -------------------------------------------------------------------
%%% Author  : Martin Huber / martin.huber@innoq.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-include("../include/mc.hrl"). 
-include_lib("stdlib/include/qlc.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_tables/0, insert_new_wish/4, selectall_wishlist/0]).

init_tables() -> 
	mnesia:create_table(wishlist, 
						[{disc_copies, [node()]},
						 {attributes, record_info(fields, wishlist)} 
						 ]),
	mnesia:create_table(rstation,
						[{disc_copies, [node()]},
						 {attributes, record_info(fields, rstation)}]).

%%-------------------------------------------------------------------------------------
%% @doc Inserts a new wishlist-entry
%% @end
%%-------------------------------------------------------------------------------------
insert_new_wish(ClientId, Artist, Title, Genre) 
  	when Artist =/= [] ->
		Lastid = highest_id(selectall_wishlist())+1,
		io:format("~p~n", [Lastid]),
		Wishlist = #wishlist{id = Lastid, clientid = ClientId, artist = Artist, title = Title, genre=Genre, saved=0},
		io:format("~p~n", [Wishlist]),
 		Fun = fun() ->
			X=mnesia:write(Wishlist),
			io:format("X:~p~n", [X]),
			X
 		end,
		mnesia:transaction(Fun).
       

%%-------------------------------------------------------------------------------------
%% @doc Does a select * from on wishlist.table
%% @end
%%-------------------------------------------------------------------------------------
selectall_wishlist() -> 
		
		{atomic, Resultlist} =  
			mnesia:transaction(
			fun() ->
				Table = mnesia:table(wishlist),
				QueryHandle = qlc:q([#wishlist{
											   id=W#wishlist.id, 
											   clientid=W#wishlist.clientid,
											   artist=W#wishlist.artist,
											   title=W#wishlist.title,
											   genre=W#wishlist.genre,
											   saved=W#wishlist.saved} || W <- Table]),
%% 				mnesia:dirty_select(wishlist, [{#wishlist{id = '_'},[],['$_']}])
				qlc:eval(QueryHandle)
			end
		),
		Resultlist.

%%--------------------------------------------
%% Internal Functions
%%--------------------------------------------

highest_id(L) -> highest_id(L, 0).                                             
highest_id([], High) -> High;
highest_id([H|T], High) -> 
		 Last = H#wishlist.id,
		 if Last > High -> 
			highest_id(T, Last);
		 true ->
			highest_id(T, High)
		 end.  


%% -------------------------------------------
%% Tests
%% -------------------------------------------
highestid_test() -> 
	WL = testwishlist(),
	18 = highest_id(WL),
	ok.


testwishlist() ->
    [#wishlist{id = 0, clientid = 1, artist = "X", title = "X said Y", genre="Genre", saved=0},
			#wishlist{id = 17, clientid = 1, artist = "XY", title = "X said Y", genre="Genre", saved=0},
			#wishlist{id = 0, clientid = 1, artist = "XYZ", title = "X said Y", genre="Genre", saved=0},
			#wishlist{id = 18, clientid = 1, artist = "A", title = "Y said X, yeah", genre="Genre", saved=0},
			#wishlist{id = 3, clientid = 1, artist = "X", title = "Y said X, yeah", genre="Genre", saved=0}
	].
