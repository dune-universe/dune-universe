-module(test_float).

-export([f/0, g/0]).

%% test case for float literal
f() -> 0.1.

%% test case for float literal using exponential notation
g() ->
    _ = 2.3e3,
    _ = 2.3e-3,
    ok.
