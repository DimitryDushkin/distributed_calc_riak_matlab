-module(dca_db_tests).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
	{foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun db_server_up/1,
	  %fun insert_data/1
	  %fun db_delete_data/1
	  fun range_request/1
	  %fun list_buckets/1
	  %fun list_bucket/1
     ]}.

setup() -> 
	{ok, Pid} = dca_db:start_link(), Pid.
cleanup(Pid) -> 
	gen_server:call(Pid, stop).

db_server_up(Pid) ->
	?_assertEqual(pong, gen_server:call(Pid, ping)).

insert_data(Pid) ->
	FilePath = "../data/data-set-1.txt",
	{timeout, 3000000,
	 	?_assertEqual(ok, gen_server:call(Pid, {insert_data, FilePath}, infinity))}.

delete_data(Pid) ->
	{timeout, 3000000,
	 	?_assertEqual(ok, gen_server:call(Pid, delete_bucket, infinity))}.

range_request(Pid) ->
	Start = dca_utils:get_timestamp(),
	BucketsListObject = gen_server:call(Pid, {get, <<"buckets">>, <<"list">>}),
	BucketsList = riakc_obj:get_value(BucketsListObject),
	Temp = string:tokens(binary_to_list(BucketsList), ","),
	[Bucket|_] = Temp,
	Reply = gen_server:call(Pid, {range_query, Bucket, "0", "2"}, infinity),
	{ok, [{0, Result}]} = Reply,
	RequestTime = dca_utils:get_timestamp() - Start,
	error_logger:info_msg("Found ~p entries~n",[length(Result)]),
 	error_logger:info_msg("Request has taken:~p ms~n",[RequestTime]),
	?_assertMatch({ok, _}, Reply).	

%% list_bucket(Pidd) ->
%% 	Start = dca_utils:get_timestamp(),
%% 	BucketsListObject = gen_server:call(Pidd, {get, <<"buckets">>, <<"list">>}),
%% 	BucketsList = riakc_obj:get_value(BucketsListObject),
%% 	Temp = string:tokens(binary_to_list(BucketsList), ","),
%% 	[Bucket|_] = Temp,
%% 	Reply = gen_server:call(Pid, {list_bucket, list_to_binary(Bucket)}, 120000),
%% 	RequestTime = dca_utils:get_timestamp() - Start,
%% 	error_logger:info_msg("Entries count: ~p~n",[length(Reply)]),
%%  	error_logger:info_msg("Request has taken:~p ms~n",[RequestTime]),
%% 	?_assertEqual(true, is_list(Reply)).
%% 	error_logger:info_msg("Result ~p~n",[Result]).


list_buckets(Pid) ->
	Start = dca_utils:get_timestamp(),
	Reply = gen_server:call(Pid, list_buckets),
	error_logger:info_msg("Result ~p~n",[Reply]),
	RequestTime = dca_utils:get_timestamp() - Start,
 	error_logger:info_msg("Request has taken:~p ms~n",[RequestTime]).