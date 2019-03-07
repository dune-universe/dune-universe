-module(test_guard).

-export([f/1]).

%% test case for guards of toplevel
f(N) when is_integer(N) -> % fun call
    0;
f(N) when erlang:is_integer(N) -> % remote fun call
    1;
f(N) when erlang:'=:='(N, 2) -> % remote fun call (binary operator)
    2;
f(N) when N =:= -1 -> % binary and unary operator
    3;
f(_) ->
    4.
