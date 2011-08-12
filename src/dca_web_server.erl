-module(dca_web_server).
-compile(export_all).

%% Folder with all web-server's related files (wsdl, etc.)
-define(WEBROOT, "../web").

start_link() ->
    {ok, spawn(?MODULE, run, [])}.

%%=============================================================
%% Start yaws web-server in embedded mode and with SOAP support
%%=============================================================
run() ->
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

	%% assume our supervisor is registered as dca_sup
	[supervisor:start_child(dca_sup, Ch) || Ch <- ChildSpecs],

	%% now configure Yaws
	yaws_api:setconf(GC, SCList),
	yaws_soap_srv:setup({dca_web_server_handler, handler}, ?WEBROOT ++ "/service.wsdl"),
	{ok, self()}.

restart() ->
	supervisor:restart_child(dca_sup, dca_web_server).


%%=============================================================
%% Generate header file for web-server from manually
%% WSDL-formatted xml file
%%=============================================================
generate_wsdl() ->
	yaws_soap_lib:write_hrl(?WEBROOT ++ "/service.wsdl",
							?WEBROOT ++ "/service.hrl").
	
