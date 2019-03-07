-module(test_spec).

-export([f/1, g/1]).

%% test case for a callback of behaviour
-callback foo(ok) -> term().

%% test case for basic spec
-spec f(A) -> integer() when A :: integer().
f(N) -> N.

%% test case for multi constraints
-spec g(A) -> B when A :: integer(),
                     B :: integer().
g(N) ->
    N * N.

%% test case for spec with module (spec of remote function)
-spec lists:member(number(), [number()]) -> boolean.
