-module(test_spec).

-export([f/1, g/1]).

%% test case for basic spec
-spec f(A) -> integer() when A :: integer().
f(N) -> N.

%% test case for multi constraints
-spec g(A) -> B when A :: integer(),
                     B :: integer().
g(N) ->
    N * N.
