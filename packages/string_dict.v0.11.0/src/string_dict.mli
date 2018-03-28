(** Efficient static string dictionaries.  By static, we mean that new key-value pairs
    cannot be added after the dictionary is created.

    This uses the algorithm the OCaml compiler uses for pattern matching on strings. *)

open Base

type 'a t [@@deriving hash, compare]

(** We don't use [[@@deriving sexp]], to avoid a circular dependency. *)
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

(** Creates a dictionary from an association list. It is an error for the list to contain
    duplicate keys. *)
val of_alist     : (string * 'a) list -> ('a t, string) Caml.result
val of_alist_exn : (string * 'a) list ->  'a t

val find     : 'a t -> string -> 'a option
val find_exn : 'a t -> string -> 'a

val to_alist : 'a t -> (string * 'a) list
