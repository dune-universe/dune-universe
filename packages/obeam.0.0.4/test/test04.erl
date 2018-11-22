-module(test04).

-export([f/0, g/1, h/1]).
-export_type([tuple/2, int/0]).

-type tuple(A, B) :: {A, B}.
-opaque int() :: integer().

f() -> (#{a => 1, b => 2})#{a := 42, c => 3}.

g(#{a := N}) -> N.

h(M) when M =:= #{a => 42} andalso M#{a := 0} =:= #{a => 0} -> M.
