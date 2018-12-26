-module(test_guard).

-export([f/1]).

%% test case for guards of toplevel
f(N) when is_integer(N) ->
    0;
f(_) ->
    2.
