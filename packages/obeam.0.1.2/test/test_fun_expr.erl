-module(test_fun_expr).

-export([f/0, g/0, h/0]).

%% test case for fun expr
f() ->
    fun (42) -> 42;
        (_) -> 57
    end.

%% test case for named fun expr
g() ->
    fun F(42) -> F(57);
        F(_) -> 57
    end.

%% test case for fun expr with guards
h() ->
    fun (N) when N =:= 42 -> N;
        (_) -> 57
    end.
