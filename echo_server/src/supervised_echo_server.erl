-module(supervised_echo_server).
-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  %% Start accepting requests
  %% We must cast this to the worker's process, as it blocks it.
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket}}.

handle_cast(accept, State = #state{socket=ListenSocket}) ->
  io:format("Waiting for client to accept...~n"),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  io:format("Client accepted...~n"),
  %% Replace this process with a newly started one
  supervised_echo_server_sup:start_socket(),
  send(AcceptSocket, "Hello", []),
  {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
  {noreply, State}.

%% Socket option active=once gives following calls
%% {tcp, Socket, Data}

%% Quit
handle_info({tcp, Socket, "quit"++_}, State) ->
  gen_tcp:close(Socket),
  {stop, normal, State};
%% Echo message
handle_info({tcp, Socket, Msg}, State) ->
  send(Socket, Msg, []),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
  io:fwrite("unexpected: ~p~n", [E]),
  {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) ->
  io:format("gen_server terminated~n"),
  ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%% Send a message back to the client
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.
