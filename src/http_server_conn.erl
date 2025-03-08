-module(http_server_conn).
-behaviour(gen_server).

-export([init/1, start_link/1, inform_error/2, inform_ok/1]).
-export([handle_info/2, handle_cast/2]).
-export([handle_call/3]).

%% Client API

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

inform_error(Pid, Why) ->
    gen_server:cast(Pid, {http_bad_parse, Why}).

inform_ok(Pid) ->
    gen_server:cast(Pid, http_good_parse).

%% Server API

% TODO: Expose an API that handles setting up the socket accept as required.
init(Socket) ->
    {ok, ParserPid} = http_parser:start_link([self()]),
    {ok, #{socket => Socket, parser => ParserPid}}.

handle_info({tcp, _Socket, Bin}, State) ->
    ParserPid = maps:get(parser, State),
    http_parser:process(ParserPid, Bin),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {ok, State}.

handle_cast({http_bad_parse, _Why}, State) ->
    io:fwrite("bad parse~n"),
    {noreply, State};

handle_cast(http_good_parse, State) ->
    Socket = maps:get(socket, State),
    gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n">>),
    gen_tcp:shutdown(Socket, write),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.
