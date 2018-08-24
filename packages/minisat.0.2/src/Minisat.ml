
(* This file is free software. See file "license" for more details. *)

type t

type 'a printer = Format.formatter -> 'a -> unit

module Lit = struct
  type t = int
  let make n = assert (n>0); n+n
  let neg n = n lxor 1
  let abs n = n land (max_int - 1)
  let sign n = if n land 1 =0 then true else false
  let to_int n = n lsr 1
  let to_string x = (if sign x then "" else "-") ^ string_of_int (to_int x)
  let pp out x = Format.pp_print_string out (to_string x)
end

type assumptions = Lit.t array

module Raw = struct
  external create : unit -> t = "caml_minisat_new"
  external delete : t -> unit = "caml_minisat_delete"

  external add_clause_a : t -> Lit.t array -> bool = "caml_minisat_add_clause_a"

  external simplify : t -> bool = "caml_minisat_simplify"

  external solve : t -> Lit.t array -> bool = "caml_minisat_solve"

  external nvars : t -> int = "caml_minisat_nvars"
  external nclauses : t -> int = "caml_minisat_nclauses"
  external nconflicts : t -> int = "caml_minisat_nconflicts"

  external set_nvars : t -> int -> unit = "caml_minisat_set_nvars"

  external value : t -> Lit.t -> int = "caml_minisat_value"

  external set_verbose: t -> int -> unit = "caml_minisat_set_verbose"
end

let create () =
  let s = Raw.create() in
  Gc.finalise Raw.delete s;
  s

exception Unsat

let check_ret_ b =
  if not b then raise Unsat

let add_clause_a s a = Raw.add_clause_a s a |> check_ret_

let add_clause_l s lits = add_clause_a s (Array.of_list lits)

let pp_clause out l =
  Format.fprintf out "[@[<hv>";
  let first = ref true in
  List.iter
    (fun x ->
       if !first then first := false else Format.fprintf out ",@ ";
       Lit.pp out x)
    l;
  Format.fprintf out "@]]"

let simplify s = Raw.simplify s |> check_ret_

let solve ?(assumptions=[||]) s =
  simplify s;
  Raw.solve s assumptions |> check_ret_

type value =
  | V_undef
  | V_true
  | V_false

let value s lit = match Raw.value s lit with
  | 1 -> V_true
  | 0 -> V_undef
  | -1 -> V_false
  | _ -> assert false

let set_verbose = Raw.set_verbose
