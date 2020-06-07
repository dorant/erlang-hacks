# Chat

## Own process registry

Start a gen_server for each chat room,
registered to a own process registry handler.

This makes it possible to call/cast to known pids.

```
rebar3 shell
registry:start_link().
chat_sup:start_room("room1").

chat:get_messages("room1").
chat:add_message("room1", "HELLO").
chat:get_messages("room1").
```

## Use gproc

```
rebar3 shell
% Should be started in app
application:start(gproc).
chat_gproc_sup:start_link().

chat_gproc_sup:start_room("room1").
chat_gproc:get_messages("room1").
chat_gproc:add_message("room1", "HELLO 1").
chat_gproc:get_messages("room1").

chat_gproc_sup:start_room("room2").
chat_gproc:add_message("room2", "HELLO 2").

chat_gproc:get_messages("room1").
chat_gproc:get_messages("room2").
```

## More info
* https://www.brianstorti.com/process-registry-in-elixir/
