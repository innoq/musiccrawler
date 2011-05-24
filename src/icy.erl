%% Author: martinh
%% Created: 23.05.2011
%% Description: TODO: Add description to mc_crawler
-module(mc_icy).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([connect/2]).

%%
%% API Functions
%%


connect(Host, Location) -> 
	GetStr = string:concat("GET ", string:concat(Location, " HTTP/1.0\r\nIcy-Metadata: 1\r\n\r\n"))	,
	io:format("~s~n", [GetStr]),				
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
	{ok, FileP} = file:open("/Users/martinh/Temp/out.str", [raw, write, binary]),
	ok = gen_tcp:send(Socket, GetStr),
	receive_data(Socket, [], FileP).

receive_data(Socket, SoFar, FileP) ->
	receive
		{tcp, Socket, Bin} ->
			file:write(FileP, Bin),
			if 
				size(SoFar) < 16000000 ->
				receive_data(Socket, [Bin|SoFar], FileP);
			true ->
				finish(Socket, SoFar, FileP)
			end;
		{tcp_closed, Socket} ->
			finish(Socket, SoFar, FileP)
	end.


finish(Sock, SoFar, FileP) ->
	file:close(FileP),
	gen_tcp:close(Sock),
	list_to_binary(lists:reverse(SoFar)).

%%
%% Local Functions
%%

