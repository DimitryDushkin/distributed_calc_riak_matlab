%%% -------------------------------------------------------------------
%%% Author  : dimitry
%%% Description :
%%%
%%% Created : 20.07.2011
%%% -------------------------------------------------------------------
-module(dca_db).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {db_pid}).

%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, dca_db}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8081),
    {ok, #state{db_pid = Pid}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% insert data in bucket named "yyyy-mm-dd-hh:mm:ss" of date added
handle_call({insert_data, FilePath}, _From, #state{db_pid = Db_pid} = State) -> 
	Bucket = dca_utils:get_date(),
	%% install riak search post-commit hook
	inets:start(),
%% 	RequestBody = {struct, [{<<"props">>,
%% 							 {struct, [{<<"precommit">>, 
%% 										{array,
%% 										 {struct, [{<<"mod">>, <<"riak_search_kv_hook">>},
%% 												   {<<"fun">>, <<"precommit">>}]}
%% 										}
%% 									  }]
%% 							 }
%% 						   }]}, 
	RequestBody = "{\"props\":{\"precommit\":[{\"mod\":\"riak_search_kv_hook\",\"fun\":\"precommit\"}]}}",
	httpc:request(put,
				  {"http://localhost:8091/riak/" ++ Bucket,	%url
				   [],						 %headers
				   "application/json",		 %content-type
				   RequestBody				 %body
				   },							
				  [], []),
	{ok, Regexp} = re:compile("([0-9.,]+)\t?"),
	TotalLines = countlines(FilePath),
	ValueCount = for_each_line_in_file(FilePath,
					fun(Line, Count) ->
						case re:run(Line, Regexp, [global, {capture, [1], list}]) of
							{match, [[Time], [TimeValue]]} ->
								Object = riakc_obj:new(list_to_binary(Bucket),
													   list_to_binary(Time),
													   list_to_binary(TimeValue),
													   <<"text/plain">>),
								ok = riakc_pb_socket:put(Db_pid, Object);
							_ -> error_logger:info_msg("Cannot parse:~p~n",[Line])
						end,
						case Count rem 10000 of
							0 -> error_logger:info_msg("Entries added:~p/~p~n",[Count, TotalLines]);
							_ -> ok
 						end,
    					Count + 1 
					end, [read], 0),
	error_logger:info_msg("Entries inserted:~p~n", [ValueCount]),
	{reply, ok, State};

%% Perform range query via simple key filters
%% @see http://wiki.basho.com/MapReduce.html#MapReduce-via-the-Erlang-API
%% @see more about pb client /deps/riakc/docs/pb-client.txt
%% @see http://wiki.basho.com/Key-Filters.html

handle_call({range_query, Bucket, From, To}, _, #state{db_pid = Pid} = State) ->	
	Result = riakc_pb_socket:search(Pid, Bucket, <<"[0 TO 1000]">>),
	{reply, Result, State};

%% handle_call({range_query, Bucket, From, To}, _, #state{db_pid = Pid} = State) ->
%% 	Query = [{map,												 	%query type
%% 			 {modfun, riak_kv_mapreduce, map_object_value},		 	%function from riak erlang built-in module
%% 			 none, true}],
%%  	Inputs = {Bucket, [[<<"between">>, list_to_binary(From), list_to_binary(To)]]},
%% 	Result = riakc_pb_socket:mapred(Pid, Inputs, Query, 120000),
%% 	{reply, Result, State};



%% Delete all entries in bucket with given name
handle_call({delete_bucket, Bucket}, _From, #state{db_pid = Db_pid} = State) ->
	%list all keys
	{ok, Keys} = riakc_pb_socket:list_keys(Db_pid, <<Bucket>>),
	TotalCount = erlang:length(Keys),
	ok = delete_keys(Keys, 0, Db_pid, Bucket, TotalCount),
	{reply, ok, State};

handle_call(ping, _From, #state{db_pid = Db_pid} = State) -> 
	Result = riakc_pb_socket:ping(Db_pid),
	{reply, Result, State};

handle_call(stop, _From, State) -> 
	{stop, normal, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% @doc delete each row and show stat
delete_keys([], _, _, _, _) -> ok;
delete_keys([Key|Rest], Acc, Db_pid, Bucket, TotalCount) ->
	Acc1 = Acc + 1,
	case Acc rem 10000 of
		0 -> error_logger:info_msg("Entries deleted:~s/~s", [Acc, TotalCount]);
		_ -> ok
 	end,
	%error_logger:info_msg("Key:~p", [Key]),
	ok = riakc_pb_socket:delete(Db_pid, Bucket, Key),
	delete_keys(Rest, Acc1, Db_pid, Bucket, TotalCount).


%% @doc read file line by line. Each line handled by Proc function
for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                for_each_line(Device, Proc, NewAccum)
    end.


%% @doc count lines in file
int_countlines(Device, Result) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Result;
        _ -> int_countlines(Device, Result + 1)
    end.
    
countlines(FileName) ->
    {ok, Device} = file:open(FileName,[read]),
    int_countlines(Device, 0).