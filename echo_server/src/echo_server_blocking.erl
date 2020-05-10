-module(echo_server_blocking).

-export([listen/1]).

-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]).

%% Start listening for connections
listen(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  accept(LSocket).

%% Wait for incoming connections and spawn an echo loop
accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket), % Blocking!
  io:format("Accepting a connection..~n"),
  spawn(fun() -> loop(Socket) end),
  accept(LSocket).

%% Echo whatever we receive
loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      gen_tcp:send(Socket, Data),
      loop(Socket);
    {error, closed} ->
      io:format("Connection closed~n"),
      ok
  end.
