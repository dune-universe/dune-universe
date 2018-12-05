-module(test10).

-export_type([any_map/0, ab_map/0]).

-type any_map() :: map().
-type ab_map() :: #{a := integer(), b => atom()}.
