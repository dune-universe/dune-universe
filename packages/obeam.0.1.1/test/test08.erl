-module(test08).

-export([f/0, f/1, f/2]).

f() ->
    F = f,

    f(1),
    F(1),
    case true of _ -> F end(1).

f(N) ->
    Test08 = test08,
    F = f,

    test08:f(N, 2),
    Test08:F(N, 2),
    case true of _ -> Test08 end:case true of _ -> F end(N, 2).

f(N, M) -> N + M.

g() ->
    Test08 = test08,
    F = f,
    Zero = 0,

    fun f/0,

    fun test08:f/0,
    fun Test08:F/Zero,

    fun (42) -> 42;
        (_) -> 57 end,
    fun F(42) -> 42;
        F(_) -> 57 end.
