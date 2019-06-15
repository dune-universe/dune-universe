-module(test_compound_pattern).

-export([f/1]).

f({A, b} = {a, B}) -> {A, B}.
