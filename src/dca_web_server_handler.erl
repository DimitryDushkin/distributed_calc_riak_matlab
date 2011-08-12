%% Author: ddushkin
%% Created: 11.08.2011
%% Description: TODO: Add description to dca_web_server_handler
%% OMG IT WORKS!!!!

-module(dca_web_server_handler).
-include("../web/service.hrl").
-compile(export_all).

handler(_Header,
        [#'p:GetDataSetsList'{'in' = In}],
        _Action, 
        _SessionValue) ->
    {ok, undefined, get_data_sets_list(In)};

handler(_Header,
        [#'p:GetDataSetValues'{'dataSetName' = Bucket}],
        _Action, 
        _SessionValue) ->
    {ok, undefined, get_data_set_values(Bucket)}.

get_data_sets_list(In) ->
	error_logger:info_msg("Input data:~p", [In]),
	Buckets = gen_server:call(dca_db, list_buckets),
	DataSetsList = lists:map(fun binary_to_list/1, Buckets),
	Response = #'p:GetDataSetsListResponse'{anyAttribs = [],
											'dataSetsList' = DataSetsList},
	[Response].

get_data_set_values(Bucket) ->
	error_logger:info_msg("Input data:~p", [Bucket]),
	Reply = gen_server:call(dca_db, {list_bucket, Bucket}).
	