`row.rets`:

```
store 0 = '0;
store 1 = '1;
store 2 = {a: ^1 | '0};
store 3 = {b: ^2 | '1};
store 4 = forall {a b} {b: a, a : b, c: b};
'2 = '3;
'2 = '4;
```

`cat test/row.rets | dune exec remu_ts --profile release`:

```
0 {b: ^2, c: ^1};
1 {a: ^1, c: ^1};
2 {a: ^1, b: ^2, c: ^1};
3 {b: ^2, a: ^1, c: ^1};
4 forall {a b} {b: a, a: b, c: b};
```