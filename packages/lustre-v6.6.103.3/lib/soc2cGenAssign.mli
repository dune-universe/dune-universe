 (* Time-stamp: <modified the 10/01/2017 (at 17:49) by Erwan Jahier> *)

(* Returns the list of non-trivial data types (i.e., arrays and
   extern) that are used in a program, with no duplicates *)
val gen_used_types : Soc.t list -> Data.t list

(* Generates a ccp macro that provides a default definition copying Data.t *)
val f: Data.t -> string

(* Ditto, but using a for loop instead of memcpy  *)
val f_forloop: Data.t -> string
