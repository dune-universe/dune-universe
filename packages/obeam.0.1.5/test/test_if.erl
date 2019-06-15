-module(test_if).

-export([f/1]).

%% test case for if expr
f(X) ->
    if
        X > 0 -> hoge;
        true -> piyo
    end.
