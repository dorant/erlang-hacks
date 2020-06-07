-module(chat_gproc_sup).

-behaviour(supervisor).

-export([start_link/0, start_room/1]).

%% Supervisor callback
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_room(Name) ->
    supervisor:start_child(?MODULE, [Name]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => chat_gproc,
                    start => {chat_gproc, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
