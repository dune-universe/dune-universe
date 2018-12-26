-module(test_fun).

-export([g/0, h/1]).

%% test cases for basic functions
f() ->
    0.

g() ->
    10.

-spec h(integer()) -> string().
h(_) ->
    "abcdefg".

-spec i(integer(), float()) -> string().
i(_, _) ->
    "abracadabra".
