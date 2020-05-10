-module(echo_server_replace).

-export([start/1]).

%% Callbacks
-export([loop/1]).

start(Port) ->
  Name = ?MODULE,
  Loop = {?MODULE, loop},
  socket_server:start(Name, Port, Loop).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      gen_tcp:send(Socket, Data),
      loop(Socket);
    {error, closed} ->
      io:format("Connection closed~n"),
      ok
  end.
