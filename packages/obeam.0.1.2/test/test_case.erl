-module(test_case).

-export([f/1]).

%% test case for basic case expr
f(R) ->
    case R of
        ok -> 1;
        error -> 2
    end.
