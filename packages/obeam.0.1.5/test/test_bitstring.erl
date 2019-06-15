-module(test_bitstring).

-export([f/0, g/1, h/1, i/0]).

%% test case for bitstring constructor expr
f() ->
    <<0,  % value
     1:1, % value and size
     2/little-signed-integer,          % value and type specifier list
     3:3/little-signed-unit:8-integer, % value, size, and type specifier list with size unit
     (2 + 2):(2 + 2) % value and size are expr
     >>.

%% test case for bitstring pattern
g(<<0, 1:1, 2/little-signed-integer, 3:3/little-signed-unit:8-integer, L:(2 + 2), B:L>>) -> B.

%% test case for bitstring constructor guard test
h(B) when B =:= <<0, 1:1, 2/little-signed-integer, 3:3/little-signed-unit:8-integer, (2 + 2):(2 + 2)>> -> ok.

%% test case for bitstring comprehension and bitstring generator
i() ->
    _ = << <<X>> || <<X>> <= <<1,2,3>>, X >= 2 >>,
    _ = << <<X>> || X <- [1,2,3], X >= 2 >>,
    _ = [ X || <<X>> <= <<1,2,3>>, X >= 2 ],
    ok.
