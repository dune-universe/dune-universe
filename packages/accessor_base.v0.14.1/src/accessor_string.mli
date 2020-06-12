open! Base
open! Import

(** Access the characters in a string as a list. *)
val list : (_, char list, string, [< isomorphism ]) Accessor.Simple.t

(** Access a reversed version of a string. *)
val reversed : (_, string, string, [< isomorphism ]) Accessor.Simple.t

(** Access each character in a string. *)
val each : (_, char, string, [< many ]) Accessor.Simple.t

(** The indexed version of [each] adds the numeric index of each character accessed to the
    index. *)
val eachi : (int * 'i -> char -> char, 'i -> string -> string, [< many ]) Accessor.t

(** Access the tuple produced by splitting a string at the given index. *)
val split_n : int -> (_, string * string, string, [< isomorphism ]) Accessor.Simple.t

(** Access the suffix of a string that begins with the given prefix, or nothing if the
    string has a different prefix. *)
val prefixed : string -> (_, string, string, [< variant ]) Accessor.Simple.t

(** Access the prefix of a string that ends with the given suffix, or nothing if the
    string has a different suffix. *)
val suffixed : string -> (_, string, string, [< variant ]) Accessor.Simple.t

(** Access the string converted to the given type, or access nothing if conversion fails.
*)
val conv
  :  (module Stringable.S with type t = 'a)
  -> (_, 'a, string, [< variant ]) Accessor.Simple.t

(** Access the string converted to the given type, raising if conversion fails. *)
val conv_strict
  :  (module Stringable.S with type t = 'a)
  -> (_, 'a, string, [< isomorphism ]) Accessor.Simple.t
