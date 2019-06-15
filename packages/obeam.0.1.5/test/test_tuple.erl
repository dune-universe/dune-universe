-module(test_tuple).

-export([f/0, g/1, h/1]).

-export_type([t/0, s/0]).

%% test case for any tuple type
-type t() :: tuple().

%% test case for tuple type
-type s() :: {atom(), integer()}.

%% test case for tuple expr
f() -> {ok, 42}.

%% test case for tuple pattern
g({ok, 42}) -> ok.

%% test case for tuple guard test
h(T) when T =:= {ok, 42} -> ok.
