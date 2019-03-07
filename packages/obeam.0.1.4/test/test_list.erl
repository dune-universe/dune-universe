-module(test_list).
-export([f/0, g/1, h/1]).

%% test case for list expressions
f() ->
    List = [3, 1, 4],
    List = [3 | [1 | [4]]],

    List = [3, 1 | [4]],
    List = [3 | [1, 4]],

    ImproperList = [3, 1 | 4],

    [X * 2 || X <- List, X >= 3].

%% test case for list patterns
g(List) ->
    [3, 1, 4] = List,
    [3 | [1 | [4]]] = List,

    [3, 1 | [4]] = List,
    [3 | [1, 4]] = List,

    [3, 1 | 4] = List.

%% test case for list guard test
h(List) when List =:= [3, 1, 4] -> ok;
h(List) when List =:= [3 | [1 | [4]]] -> ok;
h(List) when List =:= [3, 1 | [4]] -> ok;
h(List) when List =:= [3 | [1, 4]] -> ok;
h(List) when List =:= [3, 1 | 4] -> ok.
