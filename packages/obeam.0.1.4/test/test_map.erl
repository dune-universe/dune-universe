-module(test_map).

-export([f/0, g/1, h/1]).

%% test case for map creation and map update
f() -> (#{a => 1, b => 2})#{a := 42, c => 3}.

%% test case for map pattern
g(#{a := N}) -> N.

%% test case for map guard test
h(M) when M =:= #{a => 42} andalso M#{a := 0} =:= #{a => 0} -> M.
