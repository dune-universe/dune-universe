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


let colon             = (30,   Right)
let arrow             = (40,   Right)
let assign            = (41,   No)
let lambda            = (42,   No)
let push_arg          = (45,   Left)
let pull_arg          = (46,   Right)
let relation          = (50,   No)
let addition          = (60,   Left)
let multiplication    = (61,   Left)
let exponentiation    = (62,   Right)
let unknown           = (100,  Left)
let application       = (200,  Left)


let map: (int * assoc) String_map.t
  =
  let open String_map in
  empty
  |> add "->" arrow
  |> add "=>" arrow
  |> add ":=" assign
  |> add ":"  colon
  |> add "|>" push_arg
  |> add "<|" pull_arg
  |> add "="  relation
  |> add "/=" relation
  |> add "<"  relation
  |> add ">"  relation
  |> add "<=" relation
  |> add ">=" relation
  |> add "+"  addition
  |> add "-"  addition
  |> add "*"  multiplication
  |> add "/"  multiplication
  |> add "^"  exponentiation


let of_string (op:string): t =
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
