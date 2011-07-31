%% Author: ddushkin
%% Created: 31.07.2011
%% Description: TODO: Add description to dca_web_tests
-module(dca_web_server_tests).
-include_lib("eunit/include/eunit.hrl").

main_test_() ->
	{foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun generate_wsdl/0
     ]}.

setup() -> 
	application:start(dca).
cleanup(_) -> 
	ok = application:stop(dca).

generate_wsdl() ->
	dca_web_server:start().