-module(http_server_conn).
-behaviour(gen_server).

-export([init/1, start_link/1]).
-export([handle_info/2, handle_cast/2]).
-export([handle_call/3]).

%% Client API

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% Server API

% TODO: Expose an API that handles setting up the socket accept as required.
init(Socket) ->
    {ok, #{socket => Socket}}.

handle_info({tcp, _Socket, Bin}, State) ->
    io:fwrite("~s~n", [Bin]),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
