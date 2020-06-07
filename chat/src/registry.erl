-module(registry).
-behaviour(gen_server).

%% {ok, Pid} = chat:start_link("").
%% registry:start_link().
%% registry:whereis_name("test").
%% registry:register_name("test", Pid).
%% registry:register_name("test2", Pid).
%% registry:unregister_name("test").
%% registry:whereis_name("test").

%% API
-export([start_link/0, register_name/2, unregister_name/1, whereis_name/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%%% API
start_link() ->
    io:format(user, "~nSTART_LINK~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_name(RoomName, Pid) ->
    gen_server:call(?MODULE, {register, RoomName, Pid}).

unregister_name(RoomName) ->
    gen_server:cast(?MODULE, {unregister, RoomName}).

whereis_name(RoomName) ->
    gen_server:call(?MODULE, {whereis, RoomName}).

send(RoomName, Message) ->
    case whereis_name(RoomName) of
        undefined -> {badarg, {RoomName, Message}};
        Pid -> Pid ! Message, Pid
    end.

%%% gen_server callbacks
init(_Args) -> {ok,  #{}}.

handle_call({register, RoomName, Pid}, _From, State) ->
    case maps:is_key(RoomName, State) of
        false ->
            %% Start monitoring the registered process
            io:format(user, "~nMONITOR:~p~n", [Pid]),
            erlang:monitor(process, Pid),
            {reply, yes, maps:put(RoomName, Pid, State)};
        true ->
            {reply, no, State}
    end;
handle_call({whereis, RoomName}, _From, State) ->
    {reply, maps:get(RoomName, State, undefined), State}.

handle_cast({unregister, RoomName}, State) ->
    {noreply, maps:remove(RoomName, State)}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    io:format(user, "~nDOWN: ~p~n", [Reason]),
    Pred = fun(_Key, Val) -> Val =/= Pid end,
    {noreply, maps:filter(Pred, State)}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
