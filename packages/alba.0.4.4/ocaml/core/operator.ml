open Fmlib
open Common

type assoc =
  | Left
  | Right
  | No



type t =
  int * assoc (* Invariant: The same precedence leads to the same assoc
                 i.e. the precedence is the key to the operator group which
                 are considered equivalent. *)


let where             = ( 10,   Left)
let comma             = ( 20,   Right)
let assign            = ( 30,   Right)
let colon             = ( 40,   Right)
let arrow             = ( 50,   Right)
let and_or            = ( 70,   Left)
let negation          = ( 80,   Right)
let push_arg          = (100,   Left)
let pull_arg          = (120,   Right)
let relation          = (140,   No)
let addition_right    = (159,   Right)
let addition          = (160,   Left)
let multiplication    = (180,   Left)
let exponentiation    = (200,   Right)

let unknown           = (300,  Left)
let application       = (400,  Left)

(* Precedences:

    expression                  parse               requirement
    --------------------------------------------------------------
    \x := exp, 3                (\x := exp), 3      comma < assign

    \x := exp: T                \x := (exp: T)      assign < colon

    exp: A -> B                 exp: (A -> B)       colon < arrow

    all p q: p => q             all p q: (p => q)   colon < arrow

    \x y := x => y              \x y := (x => y)    assign < arrow


    Therefore:

        comma < assign < colon < arrow
*)



let map: (int * assoc) String_map.t
  =
  let open String_map in
  empty
  |> add ","   comma
  |> add "->"  arrow
  |> add "=>"  arrow
  |> add ":="  assign
  |> add ":"   colon
  |> add "and" and_or
  |> add "or"  and_or
  |> add "not" negation
  |> add "|>"  push_arg
  |> add "<|"  pull_arg
  |> add "="   relation
  |> add "/="  relation
  |> add "<"   relation
  |> add ">"   relation
  |> add "<="  relation
  |> add ">="  relation
  |> add "+:"  addition_right
  |> add "+"   addition
  |> add "-"   addition
  |> add "*"   multiplication
  |> add "mod" multiplication
  |> add "/"   multiplication
  |> add "^"   exponentiation




let is_unary (op: string): bool =
    op = "not" || op = "-"


let is_binary (op: string): bool =
    op <> "not"


let is_keyword_operator (op: string): bool =
    op = "and"
    || op = "or"
    || op = "not"
    || op = "mod"




let of_string (op: string): t =
    match
        String_map.maybe_find op map
    with
    | None ->
        unknown

    | Some d ->
        d



let compare ((prec1,_): t) ((prec2,_): t): int =
  Stdlib.compare prec1 prec2


let precedence = fst

let associativity = snd


let is_left_leaning
    ((prec1, assoc): t)
    ((prec2, _)    : t)
    : bool
=
    prec1 > prec2
    ||
    (prec1 = prec2 && assoc = Left)




let is_right_leaning
    ((prec1, assoc): t)
    ((prec2, _)    : t)
    : bool
=
    prec1 < prec2
    ||
    (prec1 = prec2 && assoc = Right)




let needs_parens
      (lower: t option)
      (is_left:bool)
      (upper: t)
    : bool
  =
  match lower with
  | None ->
     false

  | Some (low_prec, low_assoc) ->
     let prec, assoc = upper
     in
     assert (prec <> low_prec || low_assoc = assoc);

     prec > low_prec
     || (prec = low_prec  (* because of the invariant lower and upper have the
                             same associativity. *)
         && (match assoc with
             | No -> true

             | Left ->
                not is_left

             | Right ->
                is_left))
