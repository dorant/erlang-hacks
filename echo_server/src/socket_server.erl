-module(socket_server).
-behavior(gen_server).

-export([start/3]).
-export([accept_loop/1]).

%% gen_server callbacks
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTIONS, [ binary,
                       {packet, 0},
                       {active, false},
                       {reuseaddr, true}
                     ]).

-record(server_state, { port,
                        loop,
                        ip=any,
                        lsocket=null}).

%% Start gen_server
start(Name, Port, Loop) ->
  State = #server_state{port = Port, loop = Loop},
  gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSocket} ->
      NewState = State#server_state{lsocket = LSocket},
      {ok, accept(NewState)};
    {error, Reason} ->
      {stop, Reason}
  end.

%% Spawn an accept loop
%% TODO: robustness using spawn_link and traps for exits
accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
  proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
  State.

accept_loop({Server, LSocket, {M, F}}) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  io:format("Accepting a connection..~n"),
  %% Tell server to create a new accept_loop since this proc will
  %% be used for the connecting client
  gen_server:cast(Server, {accepted, self()}),
  %% Call the loop function
  M:F(Socket).

%% Create a new accept loop, replacing old
handle_cast({accepted, _Pid}, State=#server_state{}) ->
  {noreply, accept(State)}.


%% Suppress warnings by non-handling
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
