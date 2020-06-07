-module(chat).

%% registry:start_link().
%% {ok, Pid} = chat:start_link("test1").
%% chat:get_messages("test2").
%% chat:get_messages("test1").
%% chat:add_message(<<"Test1">>).
%% chat:get_messages().
%% chat:add_message(<<"Test2">>).
%% chat:get_messages().


-behaviour(gen_server).

%% API
-export([start_link/1, add_message/2, get_messages/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state, {messages = []}).

%%% API
start_link(Name) ->
    gen_server:start_link(via_tuple(Name), ?MODULE, [], []).

add_message(RoomName, Message) ->
    gen_server:cast(via_tuple(RoomName), {add_message, Message}).

get_messages(RoomName) ->
    gen_server:call(via_tuple(RoomName), get_messages).

via_tuple(Name) ->
    {via, registry, Name}.

%%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call(get_messages, _From, State) ->
    Reply = State#state.messages,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_message, NewMessage}, #state{messages = Messages} = State) ->
    {noreply, State#state{messages = Messages ++ [NewMessage]}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%% Internal functions
