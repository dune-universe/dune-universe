(********************************************************************************)
(** This file is an extension for the Environment module from the apron Library *)
(********************************************************************************)

include Apron.Environment
(** It only adds function, nothing is removed *)

(** environment build from variable string names *)
let make_s ints reals =
  make
    (Array.map Apron.Var.of_string ints)
    (Array.map Apron.Var.of_string reals)

(** empty environment *)
let empty = make [||] [||]

(** adds a variable to an environment according to its type *)
let add_one (var,typ) e =
  match typ with
  | INT -> add e [|var|] [||]
  | REAL -> add e [||] [|var|]

(** same as add_one but with string instread of Var.t *)
let add_one_s (var,typ) e = add_one ((Apron.Var.of_string var),typ) e

(** adds an interger variable *)
let add_int v = add_one (v,INT)

(** same as add_one but with string instread of Var.t *)
let add_int_s v = add_one (Apron.Var.of_string v,INT)

(** adds a real variable *)
let add_real v = add_one (v,REAL)

(** adds a real variable *)
let add_real_s v = add_one (Apron.Var.of_string v,REAL)

(** join two environments *)


let join =
  let module VarSet = struct
      type t = Apron.Var.t
      let compare = (Stdlib.compare: Apron.Var.t -> Apron.Var.t -> int)
    end
  in
  let module VS = Set.Make(VarSet) in
  fun a b ->
  let int_a,real_a = vars a and int_b,real_b = vars b in
  let ints  =
    List.fold_left
      (fun acc x -> (
         if (VS.mem x acc) then acc
         else VS.add x acc
       )
      ) VS.empty ((Array.to_list int_a) @ (Array.to_list int_b))
  in
  let reals =
    List.fold_left
      (fun acc x -> (
         if (VS.mem x acc) then acc
         else VS.add x acc
       )
      ) VS.empty ((Array.to_list real_a) @ (Array.to_list real_b))
  in
  let withreals = VS.fold add_real reals empty in
  VS.fold add_int ints withreals

(** Envrironment folding function *)
let fold f a e =
  let i,r = vars e in
  let part = Array.fold_left f a i in
  Array.fold_left f part r

(** Iterat a function over an environment *)
let iter f e   =
  fold (fun () x -> f x) () e

(** returns the array of integer variables *)
let get_ints e =
  let g,_ = vars e in g

(** returns the array of real variables *)
let get_reals e =
  let _,g = vars e in g
