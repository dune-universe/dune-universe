open Js_of_ocaml
open Js
open Mjs

(** navigation ML object *)
type push_obj = {
  pu_path : string option;
  pu_name : string option;
  params : (string * any) list option;
  query : (string * string) list option;
}

(** navigation ML argument : string ot object *)
type push_arg = PStr of string | PObj of push_obj

(** navigation iterator argument for 'next' *)
type 'a next =
  | NUnit | NFalse | NRoute of push_obj | NError of error t
  | NFunction of ('a t -> unit)

(** navigation JS object signature *)
class type push_args = object
  method path : js_string t optdef readonly_prop
  method name : js_string t opt readonly_prop
  method params : any table optdef readonly_prop
  method query : js_string t table optdef readonly_prop
end

(** JS to ML *)
let to_push_obj (p : push_args t) = {
  pu_path = to_optdef to_string p##.path;
  pu_name = to_opt to_string p##.name;
  params = to_optdef Table.items p##.params;
  query = to_optdef (Table.itemsf to_string) p##.query }

(** ML to JS *)
let of_push_obj p : push_args t = object%js
  val path = optdef string p.pu_path
  val name = opt string p.pu_name
  val params = optdef Table.make p.params
  val query = optdef (Table.makef string) p.query
end

(** empty navigation object *)
let empty = {pu_path = None; pu_name = None; params = None; query = None}

(** wrapper for navigation guards that don't have access to this *)
let wrap_hook f =
  wrap_callback (fun to_ from next ->
      let next_arg = match f (to_push_obj to_) (to_push_obj from) with
        | NUnit -> to_any ()
        | NFalse -> to_any _false
        | NRoute r -> to_any (of_push_obj r)
        | NError e -> to_any e
        | NFunction f -> to_any @@ wrap_callback f in
      next next_arg)

(** wrapper for navigation guards that have access to this *)
let wrap_meth_hook f =
  wrap_meth_callback (fun this to_ from next ->
      let next_arg = match f this (to_push_obj to_) (to_push_obj from) with
        | NUnit -> to_any ()
        | NFalse -> to_any _false
        | NRoute r -> to_any (of_push_obj r)
        | NError e -> to_any e
        | NFunction f -> to_any @@ wrap_callback f in
      next next_arg)
