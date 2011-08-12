
-module(dca_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Dca_web_server = ?CHILD(dca_web_server, worker),
	Dca_db = ?CHILD(dca_db, worker),
    {ok, {{one_for_one, 5, 10}, [Dca_web_server, Dca_db]}}.

