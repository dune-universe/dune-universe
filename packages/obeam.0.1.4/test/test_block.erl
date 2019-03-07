-module(test_block).

-export([f/0]).

%% test case for block expression
f() ->
    begin
        N = 1,
        M = 2,
        N + M
    end.
