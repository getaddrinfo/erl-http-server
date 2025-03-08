-module(http_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
			id => http_server_accepter_sup,
			start => {http_server_accepter_sup, start_link, []}
		},
		#{
			id => http_server_conn_sup,
			start => {http_server_conn_sup, start_link, []}
		}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
