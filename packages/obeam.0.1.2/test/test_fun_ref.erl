-module(test_fun_ref).

-export([f/0, g/0]).

%% test case for local fun ref
f() -> fun f/0.

%% test case for remote fun ref
g() ->
    M = test_fun_ref,
    F = f,
    A = 0,

    _ = fun test_fun_ref:f/0,
    fun M:F/A.
