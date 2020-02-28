# Three Element Group

The theory for a three element group is geometric.  The elements are
`a`, `b`, `c`.  Recall that to specify that `X` is universally
quantified when it occurs only the consequence of an axiom, the
formula `X = X` is added to the antecedent.

```
X = X => X = a | X = b | X = c.
```

Ensure the elements are distinct.

```
a = b => false. b = c => false. c = a => false.
```

The usual axioms for a group follow.

* Associativity
```
X = X & Y = Y & Z = Z
  => dot(dot(X, Y), Z) = dot(X, dot(Y, Z)).
```

* Left and right unit element exists

```
X = X => dot(X, a) = X.

X = X => dot(a, X) = X.
```

* Each element has an inverse

```
X = X => dot(X, inv(X)) = a.
```

\[[Prev](non_minimal.md)\]
\[[Up](README.md)\]
