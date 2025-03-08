-module(http_parser).
-behaviour(gen_statem).

-export([callback_mode/0]).
-export([init/1]).

callback_mode() -> state_functions.

init(_Args) -> ok.


