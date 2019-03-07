-module(test_catch_expr).

-export([f/0]).

%% test case for catch expr
f() ->
  catch error(foo).
