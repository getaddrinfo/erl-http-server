-module(http_server_accepter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    % We upgrade an accepted connection to be `{active, true}` in the
    % connection gen_server.
    {ok, ListenSocket} = gen_tcp:listen(5678, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {backlog, 256}]),

    SupFlags = #{strategy => one_for_one, intensity => 0, period => 1},
    ChildSpecs = [#{
        id => {http_server_accepter, N},
        start => {http_server_accepter, start_link, [ListenSocket]}
    } || N <- lists:seq(1, 10)],
    {ok, {SupFlags, ChildSpecs}}.
