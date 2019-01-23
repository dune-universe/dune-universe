-module(test_integer_range_type).

-export_type([t/0]).

%% test case for an integer range type
-type t() :: 1 .. 2.
