-module(test03).

f(N) when is_integer(N) ->
    0;
f(_) ->
    2.

g(R) ->
    case R of
        ok -> 1;
        error -> 2
    end.
