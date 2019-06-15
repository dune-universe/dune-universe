-module(test_op_pattern).

-export([f/1, g/1]).

%% test case for binary operator pattern
f("abc" ++ T) -> T;
f(1 + 2) -> 3.

%% test case for unary operator pattern
g(-1) -> -1.
