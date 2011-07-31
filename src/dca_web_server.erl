-module(dca_web_server).
-export([start/0, generate_wsdl/0]).

-include("../web/service.hrl").

-define(WEBROOT, "../web").

start() ->
	Id = "yaws_web_server",
	WebRoot = ?WEBROOT,
	GconfList = [{enable_soap, true},
				 {logdir, WebRoot},
            	 {id, Id}],
	SconfList = [{docroot, WebRoot},
           	     {port, 8888},
            	 {listen, {127,0,0,1}}],
	{ok, SCList, GC, ChildSpecs} =
    	yaws_api:embedded_start_conf(WebRoot, SconfList, GconfList, Id),

	%% assume our supervisor is registered as my_sup
	[supervisor:start_child(dca_sup, Ch) || Ch <- ChildSpecs],

	%% now configure Yaws
	yaws_api:setconf(GC, SCList).

generate_wsdl() ->
	yaws_soap_lib:write_hrl(?WEBROOT ++ "/service.wsdl",
							?WEBROOT ++ "/service.hrl").
	
