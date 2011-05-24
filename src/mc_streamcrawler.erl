%%%-------------------------------------------------------------------
%%% @author martinh <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%% @end
%%%-------------------------------------------------------------------

-module(mc_streamcrawler). 
-behaviour(gen_server).

%% API
-export([
         start_link/3,
         start_link/2,
         stop/0
         ]).   

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 80).

-record(state, {port, sofar, filep, lsock, gotheader=false, metaint=0}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Port, Host, Location) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, Host, Location], []).

%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' using the default port.      
start_link(Host, Location) ->
	start_link(?DEFAULT_PORT, Host, Location).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).
                                        
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port, Host, Location]) -> 
	GetStr = string:concat("GET ", string:concat(Location, " HTTP/1.0\r\nIcy-Metadata: 1\r\n\r\n"))	,
	io:format("~s~n", [GetStr]),				
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}, {packet_size, 16384}]),
	{ok, FileP} = file:open("/Users/martinh/Temp/out.str", [raw, write, binary]),
	ok = gen_tcp:send(Socket, GetStr),
	io:format("i:~p~n", [Socket]),
    {ok, #state{port = Port, sofar=[], lsock = Socket, filep=FileP}}.

handle_call(_,_,State) ->
	io:format("hc:~p~n", [call]),
	{reply, {ok, State}}.

handle_cast(stop, State) ->
	io:format("hc:~p~n", [cast]),
    {stop, normal, State}.

handle_info({tcp, _Socket, Bin}, State) ->	
	Size = size(Bin),
	case State#state.gotheader of
		true -> 
			file:write(State#state.filep, Bin),
			{noreply, State#state{sofar = [Bin|State#state.sofar]}};
		false -> 
			case analyzeHeaders(Bin) of 
				{ok, MetaInt} -> 	io:format("~nMetaInt:~p~n", [MetaInt] ),
									{noreply, State#state{sofar = [Bin|State#state.sofar], gotheader=true, metaint=MetaInt}};
				{notfound}	  ->	file:write(State#state.filep, Bin),
									{noreply, State#state{sofar = [Bin|State#state.sofar]}}
			end
	end;

handle_info({tcp_closed, _Socket}, State) ->
	io:format("hi:~p~n", [tcp_closed]),
	stop(),
	{noreply, State};

handle_info(timeout, State) ->
	io:format("hi:~p~n", [timeout]),
	stop(),
    {noreply, State}.

terminate(_Reason, State) -> 
	finish(State#state.lsock, State#state.sofar, State#state.filep),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.      


%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Finishing operations. Closing tcp connection and file-pointer & Stuff 
%% 					
%%
%% @end
%%--------------------------------------------------------------------
finish(Sock, SoFar, FileP) ->
	file:close(FileP),
	gen_tcp:close(Sock),
	list_to_binary(lists:reverse(SoFar)).

%%--------------------------------------------------------------------
%% @doc Analyze every header for some specific value and returning 
%% 					
%%
%% @end
%%--------------------------------------------------------------------
analyzeHeaders(Bin) ->
	ListOfHeaders = string:tokens(binary_to_list(Bin), "\r\n"),
	case analyzeOfO(ListOfHeaders) of 
		{ok, Value} -> {ok, Value};
		{notfound} ->  {notfound}
	end.


%%-------------------------------------------------------------------------------------
%% @doc Analyze One for one header of header-line-list for the "icy-metaint: " - value 
%% 					
%%
%% @end
%%------------------------------------------------------------------------------------

analyzeOfO([]) 		
		-> {notfound};
analyzeOfO([H|T]) 	
  		-> case (string:str(H, "icy-metaint:") > 0) of
			   true -> 
			   			Mis = string:strip(string:substr(H, 1+string:str(H, ":"))),
						{MetaInt, _} = string:to_integer(Mis),
						{ok, MetaInt};
			   false-> analyzeOfO(T) 
			end.




