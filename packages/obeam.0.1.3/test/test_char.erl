-module(test_char).

-export([f/0, g/0]).

%% test case for char literal (ASCII)
f() -> $a.

%% test case for char literal (unicode)
g() -> $あ.
