ppx_pbt
=========
Syntax extension for writing property based tests in OCaml code using properties
and generators abstraction.

New syntactic constructs
--------------------------
```ocaml
let <name> <args> = <expr>
[@@pbt {| <payload> |}]
```

Payload is defined as follow:
```
<payload> = <property> { ; <property> }

<property> = property_name <args> <gens>

<args> := { arg_identifier { , <args> } }
| _ 

<gens> = [ gen_identifier { , <gens> } ]
| _
```

Exhaustive list of [property](https://gitlab.com/vch9/ppx_pbt/-/tree/dev/src/pbt/pbt.mli):
```
commutative                [gen, gen]
associative                [gen, gen, gen]
neutral_left    {neutral}  [gen]
neutral_right   {neutral}  [gen]
neutrals        {neutral}  [gen]
capped_left     {cap}      [gen]
capped_right    {cap}      [gen]
capped          {cap}      [gen]
eq_with_f       {f}        [gen, gen]
absorb_left     {absorb}   [gen]
absorb_right    {absorb}   [gen]
absorbs         {absorb}   [gen]
floored_left    {floor}    [gen]
floored_right   {floor}    [gen]
floored         {floor}    [gen]
```

Exhaustive list of [gen](https://gitlab.com/vch9/ppx_pbt/-/tree/dev/src/pbt/pbt.mli):
```
int
uint
```

Usage
---------

### Using builtin properties and generators

```ocaml
let add x y = x + y
[@@pbt {| commutative[int, int] |}]

(* which becomes *)

let add x y = x + y

let test_add_is_commutative =
  Test.make ~name:add_is_commutative
  (QCheck.pair Pbt.Gens.int Pbt.Gens.int)
  (fun (x, y) -> Pbt.Properties.commutative add x y)

let _ =
  QCheck_run.run_tests ~verbose:true [ test_add_is_commutative ]
```

Use your own property
-----------------------

Local properties can be used instead of builtin properties:

```ocaml
let even f x = (f x) mod 2 = 0

let add x y = x + y
[@@pbt {| is_even[int] |}

(* which becomes *)

let even f x = (f x) mod 2 = 0

let inc x = x + 2

let test_inc_is_even =
  QCheck.Test.make ~name:inc_is_even
  Pbt.Gens.int
  (fun x -> even inc x)

let _ =
  QCheck_run.run_tests ~verbose:true [ test_inc_is_even ]
```

Properties comes either from:
* the exhaustive list of property
* your local definition

The local property must returns a boolean and takes as much paramaters that are
given in *arguments* and *generators*, it is translated into.

```ocaml
let test_<tested_function>_is_<your_property> =
  QCheck.Test.make ~name:<tested_function>_is_<your_property>
  <generators>
  (fun <pattern> ->
    <your_property>
    <tested_function>
    <arg0> ... <argN>
    <gen0> ... <genN>)
```

Use your own generator
------------------------

Same mecanism is available for generators:

```ocaml
let abs_int = QCheck.map (fun x -> abs x) QCheck.int

let positive f x y = (f x y) > 0

let mul x y = x * y
[@@pbt {| positive[abs_int, abs_int] |}]

(* which becomes *)

let abs_int = QCheck.map (fun x -> abs x) QCheck.int

let positive f x y = (f x y) > 0

let mul x y = x * y

let test_mul_is_positive =
  QCheck.Test.make ~name:"mul_is_positive"
  (QCheck.pair abs_int abs_int)
  (fun (x, y) -> positive mul x y)
  
let _ =
  QCheck_run.run_tests ~verbose:true [ test_mul_is_positive ]
```

Use properties with arguments
--------------------------------

Property based tests needs generators in order to check properties, but
additional arguments can also be our function parameters.

```ocaml
let zero = 0

let add x y = x + y
[@@pbt {| neutrals{zero}[int] |}]

(* which becomes *)

let test_add_is_neutrals =
  Test.make ~name:add_is_neutrals
  Pbt.Gens.int
  (fun (x, y) -> Pbt.Properties.neutrals add x)

let _ =
  QCheck_run.run_tests ~verbose:true [ test_add_is_neutrals ]
```

Property's arguments are the first parameters given to the property:

```ocaml
<property> <args> <gens> is translated as:

let ... =
  QCheck.Test.make
  ~name:"..."
  <gens>
  (fun <pattern>
    <call_to_property>
	<arg0> .. <argN>
	<gen0> .. <genN>)
```

Arguments are inlined with `OCaml.identifer`, therefore the argument must be
available at the scope of our generated test.
