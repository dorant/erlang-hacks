%%%-------------------------------------------------------------------
%% @doc redisapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(redisapp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    MaxRestart = 9999999999,
    MaxTime = 5,
    Procs = [{eredis_pool,
              {eredis_pool_sup, start_link, [1, local]},
              permanent, 5000, supervisor, [dynamic]}],
    {ok, {{one_for_all, MaxRestart, MaxTime}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
