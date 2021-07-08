(* Time-stamp: <modified the 06/03/2020 (at 11:51) by Erwan Jahier> *)

type ident = string
type v = I of int | F of float | B of bool 
       | E of ident * int
       | A of v array | S of (ident * v) list | U
       | Str of string

type t = 
  | Bool | Int | Real
  | Extern of ident
  | Enum   of (ident * ident list)
  | Struct of ident * (ident * t) list
  | Array  of (t * int)
  | Alpha of int 
  | Alias of (string * t)
  | String


val val_to_string : (float -> string) -> v -> string
val val_to_rif_string : (float -> string) -> v -> string
val val_to_string_type : v -> string
val type_of_string : string -> t
val type_to_string : t -> string

(** use the alias for aliased types *)
val type_to_string_alias : t -> string


type vntl = (string * t) list
type subst = (string * v) 

type access = Idx of int | Fld of ident | Sle of int * int * int * int

(* Replace access(pre_v) by v in pre_v *)
val update_val : v -> v -> access list -> v

(* The same as update_val in the case where no previous value exists *)
val create_val : t -> v -> access list -> v
