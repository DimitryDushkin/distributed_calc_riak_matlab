%% Author: ddushkin
%% Created: 31.07.2011
%% Description: TODO: Add description to dca_web_tests
-module(dca_web_server_tests).
-include_lib("eunit/include/eunit.hrl").
-define(WEBROOT, "../web").

main_test_() ->
	{foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun generate_wsdl/0,
	  fun restart_server/0,
	  fun check_started/0,
	  fun get_data_sets_list/0
     ]}.

setup() -> 
	application:start(dca).
cleanup(_) -> 
	ok = application:stop(dca).


check_started() ->
	Children = supervisor:which_children(dca_sup),
	lists:foreach(fun (Child) ->
					case Child of
						{dca_web_server, _, _, _} ->
							?_assertEqual(dca_web_server, dca_web_server);
						_ -> ok
					end
				  end, Children).
	
generate_wsdl() ->
	Result = dca_web_server:generate_wsdl(),
	?_assertEqual(ok, Result).

restart_server() ->
	Result1 = dca_web_server:restart(),
	?_assertMatch({ok, _}, Result1).

get_data_sets_list() ->
	inets:start(),
	WsdlPath = ?WEBROOT ++ "/service.wsdl",
	Result = yaws_soap_lib:call(WsdlPath, "GetDataSetsList", ["inputdata"]),
	error_logger:info_msg("Result:~p", [Result]),
	?_assertMatch({ok, _, [{'p:GetDataSetsListResponse', _, _}]},
				   Result).
	