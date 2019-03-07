-module(test_receive).

-export([f/0, g/0]).

%% test case for receive expression
f() ->
    receive
        ok -> ok;
        error -> error
    end.

%% test case for receive-after expression
g() ->
    receive
        ok -> ok;
        error -> error
    after
        1 + 1 -> ok
    end.
