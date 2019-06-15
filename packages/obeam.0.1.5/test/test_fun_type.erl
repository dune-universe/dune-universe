-module(test_fun_type).

-export_type([t/0, u/0, v/0, w/0]).

%% test case for a function type
-type t() :: fun().     %% any function
-type u() :: fun((...) -> integer()).     %% any arity, returning Type
-type v() :: fun(() -> integer()).
-type w() :: fun((integer()) -> integer()).
