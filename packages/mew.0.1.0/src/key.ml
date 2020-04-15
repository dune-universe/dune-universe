(*
 * key.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew.
 *)

let hash code modifier=
  let shift= List.length modifier in
  let code= code lsl shift in
  let modifier= List.mapi (fun i m-> m lsl i) modifier in
  let key= List.fold_left (fun sum m-> sum land m) code modifier in
  let result=
    let key= float_of_int key in
    mod_float (key *. 0.6180339887) 1. *. 16384. |> int_of_float in
  result


module type S = sig
  type t

  type code
  type modifier
  type modifiers

  val create : code:code -> modifiers:modifiers -> t
  val create_modifiers : modifier list -> modifiers

  val code : t -> code
  val modifiers : t -> modifiers
  val modifier : key:t -> modifier:modifier -> bool

  val compare : t -> t -> int

  val to_string : t -> string

  include Hashtbl.HashedType with type t:= t
end

