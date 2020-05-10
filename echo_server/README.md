# Echo server designs

## Blocking

Runs the accept part in current process, where it will block.

### Files

```
src/echo_server_blocking.erl
```

### Run

```
rebar3 shell
echo_server_blocking:listen(23001).

telnet localhost 23001
```

## Spawn and replace

Spawning of a processes that will block in accept.
When a client connects the process will trigger a new process to handle next accept,
and continue handle the client.

### Files

```
src/echo_server_replace.erl
src/socket_server.erl
```

### Run

```
rebar3 shell
echo_server_replace:start(23001).
observer:start().

telnet localhost 23001
```

## Multiple client listeners

Multiple processes can listen on the socket (via accept)
When a client is accepted a new listerner is spawned to replace the one handling the client.
Using supervision to uphold the listeners.

Important part: simple_one_for_one
See: https://learnyousomeerlang.com/buckets-of-sockets

### Files

```
src/supervised_echo_server_sup.erl
src/supervised_echo_server.erl
```

### Run

```
rebar3 shell
supervised_echo_server_sup:start_link(23001).

telnet localhost 23001
Enter `quit` in telnet session to close connection
```
