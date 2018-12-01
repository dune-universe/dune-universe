-module(test08).

-export([f/0, f/1, f/2]).

f() ->
    f(1).

f(N) ->
    test08:f(N, 2).

f(N, M) -> N + M.
