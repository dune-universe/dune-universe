-module(test_map_type).

-export_type([any_map/0, ab_map/0]).

%% test case for any map type
-type any_map() :: map().

%% test case for map type
-type ab_map() :: #{a := integer(), b => atom()}.
