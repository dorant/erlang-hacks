-module(supervised_echo_server_sup).
-behaviour(supervisor).

-export([start_link/1]).

%% Callback
-export([start_socket/0]).

%% Supervisor callback
-export([init/1]).

%% Using lists instead of binary!
-define(TCP_OPTIONS, [{active, once},
                      {reuseaddr, true}]).

start_link(Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  io:format("Init port=~p ListenSocket=~p~n", [Port, ListenSocket]),
  %% Start a pool of client listeners
  %% Do this in another process since it is blocking
  spawn_link(fun client_listeners/0),
  {ok, { {simple_one_for_one, 60, 3600},
         [
          {supervised_echo_server,
           {supervised_echo_server, start_link, [ListenSocket]},
           temporary, 1000, worker, [supervised_echo_server]}
         ]
       } }.

%% Start a child
%% Used at startup and later when replacing a client handler
start_socket() ->
  supervisor:start_child(?MODULE, []).

%% Start 5 listeners to be able to handle multiple connections
client_listeners() ->
  [start_socket() || _ <- lists:seq(1,5)],
  ok.
