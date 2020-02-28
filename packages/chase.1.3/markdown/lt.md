# Less Than

This theory axiomatizes the less than relation on the natural numbers
below five.  For natural numbers, `o` is zero and `s` is the successor
function.

Axiom 0 states that 0 < 4.
```
lt(o, s(s(s(s(o))))).             %(0)
```

x < y + 1 => y < y + 1.
```
lt(X, s(Y)) => lt(Y, s(Y)).       %(1)
```

x + 1 < y + 1 => x < y.
```
lt(s(X), s(Y)) => lt(X, Y).       %(2)
```

The less than relation is transitive.
```
lt(X, Y) & lt(Y, Z) => lt(X, Z).  %(3)
```

Chase finds the expected model.

## Exercise

Try finding models for this theory after omitting Axiom 1.  Why does
Chase not find the expected model?  Hint: use `chasetree` to view the
steps taken by Chase when finding models for the two theories.

\[[Prev](total_ordering.md)\]
\[[Up](README.md)\]
\[[Next](non_minimal.md)\]
