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

-define(DEFAULT_BUCKET_NAME, "dca2").

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
handle_call(insert_data, _From, #state{db_pid = Db_pid} = State) -> 
	{ok, Regexp} = re:compile("[0-9.,]+\t?"),
	ValueCount = for_each_line_in_file("../data/data-set-1.txt",
						  fun(Line, Count) ->
							case re:run(Line, Regexp, [global, {capture, [1], binary}]) of
								{match, [Time, Value]} ->
									Body = mochijson2:encode({struct, [
										 {<<"set_id">>, <<"1">>},
										 {<<"time">>, Time},
										 {<<"value">>, Value}
										]}),
									%put object to "dca" bucket with generated key
									Object = riakc_obj:new(<<?DEFAULT_BUCKET_NAME>>, undefined, Body),
									{ok, _} = riakc_pb_socket:put(Db_pid, Object);
								_ -> error_logger:info_msg("Cannot parse:~s",[Line])
							end,
							case Count rem 10000 of
								0 -> error_logger:info_msg("Entries added:~s",[Count]);
								_ -> ok
 							end,
    				   		Count + 1 
						  end, [read], 0),
	error_logger:info_msg("Entries inserted:~s", [ValueCount]),
	{reply, ok, State};

handle_call(delete_bucket, _From, #state{db_pid = Db_pid} = State) ->
	%list all keys
	{ok, Keys} = riakc_pb_socket:list_keys(Db_pid, <<?DEFAULT_BUCKET_NAME>>),
	TotalCount = erlang:length(Keys),
	ok = delete_keys(Keys, 0, Db_pid, TotalCount),
%% 	lists:foreach(fun(Key) ->
%% 				  	%delete 
%% 					riakc_pb_socket:delete(Db_pid, ?DEFAULT_BUCKET_NAME, Key)
%% 				  end, Keys),
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
delete_keys([], _, _, _) -> ok;
delete_keys([Key|Rest], Acc, Db_pid, TotalCount) ->
	Acc1 = Acc + 1,
	case Acc rem 10000 of
		0 -> error_logger:info_msg("Entries deleted:~s/~s", [Acc, TotalCount]);
		_ -> ok
 	end,
	%error_logger:info_msg("Key:~s", [Key]),
	ok = riakc_pb_socket:delete(Db_pid, <<?DEFAULT_BUCKET_NAME>>, Key),
	delete_keys(Rest, Acc1, Db_pid, TotalCount).


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
