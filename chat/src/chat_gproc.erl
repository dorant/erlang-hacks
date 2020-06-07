-module(chat_gproc).
-behaviour(gen_server).

%% API
-export([start_link/1, add_message/2, get_messages/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state, {messages = []}).

-define(SERVER(UserId), {via, gproc, {n, l, {?MODULE, UserId}}}).

%%% API
start_link(Name) ->
    gen_server:start_link(?SERVER(Name), ?MODULE, [], []).

add_message(RoomName, Message) ->
    gen_server:cast(?SERVER(RoomName), {add_message, Message}).

get_messages(RoomName) ->
    gen_server:call(?SERVER(RoomName), get_messages).


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
