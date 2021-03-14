(********************************************************************************)
(** This file is an extension for the Environment module from the apron Library *)
(********************************************************************************)

(** It only adds function, nothing is removed *)
include Apron.Environment

(** environment build from variable string names *)
let make_s ints reals =
  make
    (Array.map Apron.Var.of_string ints)
    (Array.map Apron.Var.of_string reals)

(** empty environment *)
let empty = make [||] [||]

exception DuplicateName of string

let add_check e ints reals =
  Array.iter
    (fun v ->
      if mem_var e v then raise (DuplicateName (Apron.Var.to_string v ^ ": int"))
      )
    ints ;
  Array.iter
    (fun v ->
      if mem_var e v then
        raise (DuplicateName (Apron.Var.to_string v ^ ": real")) )
    reals ;
  add e ints reals

(** adds a variable to an environment according to its type *)
let add_one (var, typ) e =
  match typ with
  | INT -> add_check e [|var|] [||]
  | REAL -> add_check e [||] [|var|]

(** same as add_one but with string instread of Var.t *)
let add_one_s (var, typ) e = add_one (Apron.Var.of_string var, typ) e

(** adds an interger variable *)
let add_int v = add_one (v, INT)

(** same as add_one but with string instread of Var.t *)
let add_int_s v = add_one (Apron.Var.of_string v, INT)

(** adds a real variable *)
let add_real v = add_one (v, REAL)

(** adds a real variable *)
let add_real_s v = add_one (Apron.Var.of_string v, REAL)

(** join two environments *)

let join =
  let module VarSet = struct
    type t = Apron.Var.t

    let compare = Apron.Var.compare
  end in
  let module VS = Set.Make (VarSet) in
  fun a b ->
    let int_a, real_a = vars a and int_b, real_b = vars b in
    let ints =
      List.fold_left
        (fun acc x -> VS.add x acc)
        VS.empty
        (Array.to_list int_a @ Array.to_list int_b)
    in
    let reals =
      List.fold_left
        (fun acc x -> VS.add x acc)
        VS.empty
        (Array.to_list real_a @ Array.to_list real_b)
    in
    empty |> VS.fold add_real reals |> VS.fold add_int ints

(** Envrironment folding function *)
let fold f a e =
  let i, r = vars e in
  let part = Array.fold_left f a i in
  Array.fold_left f part r

(** Iterat a function over an environment *)
let iter f e = fold (fun () x -> f x) () e

(** returns the array of integer variables *)
let get_ints e =
  let g, _ = vars e in
  g

(** returns the array of real variables *)
let get_reals e =
  let _, g = vars e in
  g
