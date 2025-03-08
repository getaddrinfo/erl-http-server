-module(http_server_accepter).
-behaviour(gen_server).

-export([init/1, start_link/1]).
-export([handle_cast/2, accept_loop/2]).
-export([handle_call/3]).

start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, ListenSocket, []).

init(ListenSocket) ->
    LoopPid = spawn_link(?MODULE, accept_loop, [self(), ListenSocket]),
    {ok, LoopPid}.

handle_call(_Request, _From, State) ->
    {stop, no_handle, State}.

handle_cast({handoff, Socket}, State) ->
    {ok, Pid} = supervisor:start_child(http_server_conn_sup, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    ok = inet:setopts(Socket, [{active, true}]),

    {noreply, State}.

% This function forever accepts connections from ListenSocket, and
% hands them off to the `http_server_accepter`, to pass off to a
% `http_server_conn`.
accept_loop(Parent, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),

    % Hand control from this accepter to the parent. The chain of control goes:
    % accept_loop -> http_server_accepter -> http_server_conn
    ok = gen_tcp:controlling_process(Socket, Parent),

    gen_server:cast(Parent, {handoff, Socket}),
    accept_loop(Parent, ListenSocket).
