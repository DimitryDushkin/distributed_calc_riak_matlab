%% Author: dimitry
%% Created: 11.06.2011
%% Description: TODO: Add description to st_utils
-module(dca_utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_timestamp/0, get_date/0]).

%%
%% API Functions
%%

%%--------------------------------------------------------------------
%% Return UNIX timestamp in ms
%%--------------------------------------------------------------------
get_timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% Return date string "yyyy-mm-dd-hh:mm:ss"
get_date() ->
	lists:flatten(iso_8601_fmt(erlang:localtime())).


iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B-~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).	
	