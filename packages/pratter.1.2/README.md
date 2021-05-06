Pratter: A parser for terms with operators and applications
===========================================================

Pratter is a library that provides a parser that transforms streams of terms to
applied terms.  Terms may contain infix or prefix operators and applications.
The parser is an extension of the Pratt parsing algorithm.

Examples can be seen in the tests inside the `t/` folder.

Example: simple terms
---------------------

The idea is that terms are made of symbols and applications and some symbol can
be declared to be infix or prefix operators.

Start by defining the terms,
``` ocaml
type term = Appl of term * term | Symb of string
```

and the data structure that maps symbols identifiers to operators properties,
```ocaml
type table = {
    unary  : (string * Pratter.priority) list
  ; binary : (string * (Pratter.priority * Pratter.associativity)) list
}
let empty : table = { unary = [] ; binary = [] }
```

Next, define a module to pack these two types, and two functions:

- `get` that is able to retrieve the properties of a symbol if it is declared as
  an operator,
- `make_appl` that creates an application out of two terms.

```ocaml
module Terms : Pratter.SUPPORT with type term = term and type table = table =
struct
  type nonrec term = term
  type nonrec table = table
  let get { unary; binary } (t: term) =
  match t with
  | Symb id -> (
      try Some (Pratter.Una, List.assoc id unary)
      with Not_found -> (
        try
          let bp, assoc = List.assoc id binary in
          Some (Bin assoc, bp)
        with Not_found -> None ) )
  | _ -> None

  let make_appl t u = Appl (t, u)
end

module Parser = Pratter.Make (Terms)
```

Then that's it, we can parse streams of terms with operators. For instance,
assume that we want to parse `x + y * -z` where

- `+` and `*` are infix operators, with `*` having a higher binding power than
  `+`;
- `-` is a unary operator having a higher binding power than `+` and `*`.

Create a table holding these operators:

``` ocaml
let tbl =
  { unary = [ "-", 1.0 ]
  ; binary = [ ("+", (0.5, Pratter.Left)) ; ("*", (0.6, Pratter.Left)) ] }
```

Priority (also called binding power) can be any float, and associativity may be
`Pratter.Left`, `Pratter.Right` or `Pratter.Neither`.

Finally parse the input using `Parser.expression`:
``` ocaml
let input = [ Symb "x"; Symb "+"; Symb "y"; Symb "*"; Symb "-"; Symb "z"]
Parser.expression tbl (Stream.of_list input)
```
we obtain the term `x + (y * (-z))` representend by
``` ocaml
Appl (Appl (Symb "+", Symb "x"),
 Appl (Appl (Symb "*", Symb "y"), Appl (Symb "-", Symb "z")))
```
