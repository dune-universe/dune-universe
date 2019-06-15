-module(test_empty_list_type).

-export_type([t/0, u/0]).

%% test case for the empty list type
-type t() :: [].
-type u() :: nil().
