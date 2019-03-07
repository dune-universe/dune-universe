-module(test_unary_op_expr).

-export([f/0]).

%% test case for unary operator expression
f() ->
    _ = not true,
    -1.
