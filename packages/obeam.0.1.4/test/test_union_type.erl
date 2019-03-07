-module(test_union_type).

-export_type([t/0]).

%% test case for a type union
-type t() :: integer() | string().
