-module(http_server_conn_sup).
-export([start_link/0]).
-export([init/1]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    ChildSpecs = [#{
        id => http_server_conn,
        start => {http_server_conn, start_link, []},
        restart => transient
    }],
    {ok, {SupFlags, ChildSpecs}}.
