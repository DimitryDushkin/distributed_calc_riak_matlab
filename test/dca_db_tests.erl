-module(dca_db_tests).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
	{foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun db_server_up/1,
	  fun db_delete_data/1
     ]}.

setup() -> 
	{ok,Pid} = dca_db:start_link(), Pid.
cleanup(Pid) -> 
	gen_server:call(Pid, stop).

db_server_up(Pid) ->
	?_assertEqual(pong, gen_server:call(Pid, ping)).

db_insert_data(Pid) ->
	{timeout, 3000000,
	 	?_assertEqual(ok, gen_server:call(Pid, insert_data, infinity))}.

db_delete_data(Pid) ->
	{timeout, 3000000,
	 	?_assertEqual(ok, gen_server:call(Pid, delete_bucket, infinity))}.