-module(test_integer_range_type).

-export_type([t/0]).

%% test case for an integer range type
-type t() :: 1 .. 2.

%% test case for an integer range type with binary operation
-type s() :: 1 band 3 * 5 .. $a.
