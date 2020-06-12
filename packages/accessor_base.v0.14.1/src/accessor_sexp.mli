open! Base
open! Import

include sig
  type t =
    | Atom of string
    | List of t list
  [@@deriving accessors]
end
with type t := Sexp.t

(** Access the immediate children of a list node. Accesses nothing for atoms. *)
val children : (_, Sexp.t, Sexp.t, [< many ]) Accessor.Simple.t

(** Treat a list node with two elements as a tuple. *)
val tuple2 : (_, Sexp.t * Sexp.t, Sexp.t, [< variant ]) Accessor.Simple.t

(** Access any immediately children that are tuples (lists with two elements), where the
    first component is an atom containing the given string. *)
val field : string -> (_, Sexp.t, Sexp.t, [< many ]) Accessor.Simple.t

(** Access the sexp if it is a list whose first element is an atom containing the given
    string. *)
val variant : string -> (_, Sexp.t list, Sexp.t, [< optional ]) Accessor.Simple.t

(** Access all the atoms in a sexp, recursively. *)
val atoms : (_, string, Sexp.t, [< many ]) Accessor.Simple.t

(** Access the sexp converted to the given type, or access nothing if conversion fails. *)
val conv
  :  (module Sexpable.S with type t = 'a)
  -> (_, 'a, Sexp.t, [< variant ]) Accessor.Simple.t

(** Access the sexp converted to the given type, raising if conversion fails. *)
val conv_strict
  :  (module Sexpable.S with type t = 'a)
  -> (_, 'a, Sexp.t, [< isomorphism ]) Accessor.Simple.t
