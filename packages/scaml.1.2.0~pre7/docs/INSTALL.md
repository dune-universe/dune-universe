# SCaml installation

Install `opam`, then:

```
$ git clone https://gitlab.com/dailambda/scaml
$ cd scaml
$ git checkout 1.2.0pre
$ opam switch create . ocaml-base-compiler.4.09.1 --no-install
$ opam install -y ./scaml.opam
```

If successful, there should be the compiler executable:

```
$ which scamlc
.../_opam/bin/scamlc
```

## Tests and examples

`src/tests` directorty contains *working* tests which you can use as examples.

```
$ dune build
$ cd tests
$ ./test.sh xxx.ml
```

or 

```
$ dune build
$ cd tests
$ ./test_all.sh
```

If `tezos-client` is in `PATH` and it is configured to connect to a running node with a valid blockchain protocol, it should also dry-run the compiled tz:

```
$ ./test.sh closure2.ml 
comp=dune exec ../main.exe --
----- closure2.ml
dune exec ../main.exe -- /Users/jun/.share/4.07.1/scaml/src/tests//_build/closure2.ml
Entering directory '/Users/jun/.share/4.07.1/scaml'
Entering directory '/Users/jun/.share/4.07.1/scaml'
parameter unit ;
storage unit ;
code { { /* defs */ } ;
       { /* entry point init */ DUP ; CDR ; DIP { CAR } } ;
       { /* entry point */
         { /* entry main */
           PUSH int 6 ;
           { /* = d */ { /* = a */ PUSH int 1 } ; { /* = c */ PUSH int 3 } ; ADD } ;
           { /* = e */ PUSH int 2 } ;
           ADD ;
           COMPARE ;
           EQ ;
           ASSERT ;
           PUSH unit Unit ;
           NIL operation ;
           PAIR } } ;
       { /* final clean up */ DIP { DROP 2 } } } ;

Executing /var/folders/6_/d6bx9d112z7fzxvwdmwk5qgr0000gn/T/tezos-tmp-client.XXXXXXXX.5M0z3d0L/bin/tezos-client run script /Users/jun/.share/4.07.1/scaml/src/tests//_build/closure2.tz on storage Unit and input Unit
storage
  Unit
emitted operations
  
big_map diff
```
