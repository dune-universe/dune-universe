-module(test_operator_type).

-export_type([t/0, u/0, v/0]).

%% test case for an operator type
-type t() :: 1 + 2.
-type u() :: 3 * 4.
-type v() :: -4.
