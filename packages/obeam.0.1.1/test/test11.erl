-module(test11).

-export([g/0]).

f() -> ok.

%% test case for FunT chunk (function table)
g() -> fun f/0.
