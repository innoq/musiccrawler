%%%-------------------------------------------------------------------
%%% @author martinh <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%% @end
%%%-------------------------------------------------------------------

-module(mc_icy). 
-behaviour(gen_server).

%% API
-export([
         start_link/4,
         start_link/2,
         stop/0
         ]).   

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 80).
-define(DEFAULT_FILE, "/tmp/out.mp3").
-define(DEFAULT_META, "/tmp/out.txt").

-record(state, {	port, 
					sofar, 
					filep, 
					lsock, 
					gotheader=false, 
					metaint=0, 
					interpret, 
					title, 
					metaoverlap=0}).

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
start_link(Port, Host, Location, File) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, Host, Location, File], []).

%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' using the default port.      
start_link(Host, Location) ->
	start_link(?DEFAULT_PORT, Host, Location, ?DEFAULT_FILE).

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

init([Port, Host, Location, File]) -> 
	GetStr = string:concat("GET ", string:concat(Location, " HTTP/1.0\r\nIcy-Metadata: 1\r\n\r\n"))	,
	io:format("~s~n", [GetStr]),				
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
	{ok, FileP} = file:open(File, [raw, write, binary]),
%	{ok, metafilep} = file:open(?, [raw, write, binary]),
	ok = gen_tcp:send(Socket, GetStr),
	io:format("i:~p~n", [Socket]),
    {ok, #state{port = Port, sofar=[], lsock = Socket, filep=FileP}}.

handle_call(_,_,State) ->
	io:format("hc:~p~n", [call]),
	{reply, {ok, State}}.

handle_cast(stop, State) ->
%	io:format("hc:~p~n", [State#state.sofar]),
    {stop, normal, State}.

handle_info({tcp, _Socket, Bin}, State) ->	
	case State#state.gotheader of
		true -> 
			{NewBin, Metadata, MetaOverlap} = extract(Bin, State),
			{Change, NewInterpret, NewTitle} = evaluateStreamtitle(Metadata, State#state.interpret, State#state.title),
			{noreply, State#state{sofar = [NewBin|State#state.sofar], interpret=NewInterpret, title=NewTitle, metaoverlap=MetaOverlap}};
		false -> 
			case analyzeHeaders(Bin) of 
 				{ok, MetaInt} -> 	%% io:format("~nMetaInt:~p~n", [MetaInt] ),
									{noreply, State#state{gotheader=true, metaint=MetaInt}};
				{notfound}	  ->	Bin = 1
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
	finish(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.      


%%%===================================================================
%%% Internal Functions
%%%===================================================================
extract(Bin, State) -> 
	New = size(Bin),
%% 	io:format("-------------~nNew_size:~p~n",[New]),
	SoFarSize = getsizeofbininlist(State#state.sofar),
	case SoFarSize =:= 0 of
		true ->  
%% 				io:format("1st_size->true~n"),
				FrameFillSize =  0;  							
				% RestSize =  State#state.metaint - FrameFillSize;
		false -> 
				FrameFillSize = SoFarSize rem State#state.metaint
%% 				if 
%% 					FrameFillSize =:= 0 ->
%% 						% Special Case, when Bin ends exactly at the border between 
%% 						% data and meta, we have to manipulate the Restsize a bit to signal 
%% 						% for immediate metahandling
%% 						RestSize =  0;
%% 					true ->
%% 						% else just calculate it.
%% 						RestSize =  State#state.metaint - FrameFillSize
%% 				end
	end,

	RestSize =  State#state.metaint - FrameFillSize,

	% FrameFillSize = Size of Data filling the most recent frame
	% Y = Room for data until the next metadata comes
	% Overlp is a possible overlap of metadata from last package to this one
	Ovrlp = State#state.metaoverlap,
	
%% 	io:format("SoFarSize:~p / FrameFillSize:~p / RestSize:~p / Overlap: ~p~n", [SoFarSize, FrameFillSize, RestSize, Ovrlp]),
	if 
		Ovrlp =:= 0, RestSize > 0 -> % When there's no overlap and room for data -> handle data
			handleData(Bin, New, RestSize);
		Ovrlp > 0 -> 		  % When there IS a overlap, handle meta with that overlap
			handleMeta(Bin, Ovrlp);
		true ->				 % so there's no overlap but also no room for data - then we handle meta normally
			handleMeta(Bin)
	end.



%%-------------------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------------------
handleData(Bin, New, RestSize) 
  	-> 
		if 
			New < RestSize ->  		% remember RestSize? If there's more/exact room for data then we give back just data
					{Bin, <<>>, 0};    
			New =:= RestSize -> 	% this is a perfect fit, but we should start with a overlap signaling the length byte (-1)
 					{Bin, <<>>, -1};	
			New > RestSize 	-> 		% so we have a bit metadata in there! Split it and give the meta to handlemeta			
				{BinTemp, MetaDataTemp} = split_binary(Bin, RestSize),
				{BinNew, MetaData, MetaOverlap} = handleMeta(MetaDataTemp),
%				io:format("~nNew Data after Metahandling:~p~n", [size(<<BinTemp/binary,BinNew/binary>>)]),
				{<<BinTemp/binary,BinNew/binary>>, MetaData, MetaOverlap}
		end.


				
%%-------------------------------------------------------------------------------------
%% @doc handle meta with no overlap. takes the first byte, and calculates that by 16 bytes, <br>
%%		the data until up to that point is meta. the rest is data and both will be given back.
%% @end
%%-------------------------------------------------------------------------------------	
handleMeta(Bin) 
  ->
	{Byte,Rest} = split_binary(Bin, 1),
	SizeOfMeta = binary:decode_unsigned(Byte) * 16,
	RestSize = size(Rest),
%% 	io:format("---~nMetahandling~nMetaSize:~p - RestSize:~p BinSize:~p~n", [SizeOfMeta, RestSize, size(Bin)]),
	if 	
		SizeOfMeta =:= 0 
		  		-> {Rest, <<>>, 0};			
		RestSize > SizeOfMeta 
		  		-> {Meta, Data} = split_binary(Rest, SizeOfMeta),
%% 				   io:format("Return:~s~n - Meta: ~p / Data:~p~n", [binary_to_list(Meta), size(Meta), size(Data)]),
				   {Data, Meta, 0};		% 0 at end means no overlap of metadata
		RestSize =:= SizeOfMeta ->
%% 		  		   io:format("Return only Meta =:=:~s~n - Meta: ~p~n", [binary_to_list(Rest), size(Rest)]),
		  		   {<<>>, Rest, 0};	   % 0 at end means no overlap of metadata, all is Meta
		RestSize < SizeOfMeta -> 
%% 					io:format("Return only Meta ovr:~s~n - Meta: ~p~n", [binary_to_list(Rest), size(Rest)]),
					{<<>>, Rest, SizeOfMeta - RestSize}  %% Calc. Value at end means is overlap of metadata, all is Meta   
	end.

			

%%-------------------------------------------------------------------------------------
%% @doc metadata with overlap is simpler, we just take the overlap, and split
%% binaries until that point. first part is meta, rest is data (music, yeah)
%% @end
%%-------------------------------------------------------------------------------------	
handleMeta(Bin, Ovrlp) 
  ->
	case Ovrlp =:= -1 of
					%% -1 is a signal for overlap starts with length byte, do a normal metahandling then
		true -> 	handleMeta(Bin);  
					 
		false ->	{Meta, Data} = split_binary(Bin, Ovrlp),
%% 					io:format("handle Meta with Overlap: ~p~n", [{size(Meta), size(Data)}]),
					{Data, Meta, 0}
	end.


%%--------------------------------------------------------------------
%% @doc Finishing operations. Closing tcp connection and file-pointer & Stuff 
%% 					
%%
%% @end
%%--------------------------------------------------------------------
finish(State) ->
    file:write(State#state.filep, list_to_binary(lists:reverse(State#state.sofar))),
	file:close(State#state.filep),
	gen_tcp:close(State#state.lsock).

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
%% @doc
%% @end
%%-------------------------------------------------------------------------------------
getsizeofbininlist(L) -> lists:sum([size(I)||I<-L]).

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


evaluateStreamtitle (Meta, InterpretOld, TitleOld) -> 
	case size(Meta) > 2 of
		true ->
				Str = string:sub_word(binary_to_list(Meta), 2, $'),
				{Int,T} = erlang:list_to_tuple([string:strip(I) || I <- string:tokens(Str, "-")]),
				io:format("~nInterpret:~p| Title:~p|~n", [Int, T]),
				Changed = true xor string:equal(Int, InterpretOld) and string:equal(T, TitleOld), 
				{Changed, Int,T};	
		false -> {false, InterpretOld, TitleOld}
	end.
		


