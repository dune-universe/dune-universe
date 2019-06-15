-module(test_fun_call).

-export([f/0, f/1, f/2]).

%% test case for local fun call
f() ->
    F = f,

    f(1),
    F(1),
    case true of _ -> F end(1).

%% test case for remote fun call
f(N) ->
    M = test_fun_call,
    F = f,

    test_fun_call:f(N, 2),
    M:F(N, 2),
    case true of _ -> M end:case true of _ -> F end(N, 2).

f(N, M) -> N + M.
