-module(test_bitstring_type).

-export_type([t/0, u/0, v/0, w/0]).

%% test case for a bitstring type
-type t() :: <<>>.
-type u() :: <<_:1>>.
-type v() :: <<_:_*2>>.
-type w() :: <<_:3, _:_*4>>.
