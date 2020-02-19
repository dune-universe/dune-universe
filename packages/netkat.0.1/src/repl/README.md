# NetKAT REPL

The NetKAT REPL allows for interactive compilation of NetKAT policies into IDDs
and forwarding tables. Currently only KAT+B! expressions are supported.

## Usage

Build the project with
```
dune build
```
and start the KAT+B! REPL with
```
dune exec -- katbb repl
```
Alternatively replace `katbb` in the above command with `katbv` to invoke the 
KAT+BV REPL.

To see the available commands type `help` in the REPL.
