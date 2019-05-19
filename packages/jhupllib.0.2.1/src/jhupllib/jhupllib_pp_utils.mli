(* This module contains utilities for pretty-printing using the Formatter
   module. *)
open Batteries;;
open Format;;

type 'a pretty_printer = (formatter -> 'a -> unit);;

(** A function to pretty-print an enumeration of items.  This enumeration is
    boxed and the delimiter is interleaved between each item.  The arguments
    are:
    - The separator string.
    - The pretty-printing function for the items.
    - The formatter to use.
    - The item enumeration.
*)
val pp_concat_sep :
  string -> 'a pretty_printer -> 'a Enum.t pretty_printer

(** A function to pretty-print an enumeration of items bracketed by specific
    start and stop symbols.  The arguments are:
    - The start symbol.
    - The end symbol.
    - The separator string.
    - The pretty-printing function for the items.
    - The formatter to use.
    - The item enumeration.
*)
val pp_concat_sep_delim :
  string -> string -> string -> 'a pretty_printer -> 'a Enum.t pretty_printer

(** Pretty-prints a tuple.  The arguments are:
    - A pretty-printing function for the first element.
    - A pretty-printing function for the second element.
    - The formatter.
    - The tuple.
*)
val pp_tuple :
  'a pretty_printer -> 'b pretty_printer -> ('a * 'b) pretty_printer

(** Pretty-prints a triple.  The arguments are:
    - A pretty-printing function for the first element.
    - A pretty-printing function for the second element.
    - A pretty-printing function for the third element.
    - The formatter.
    - The triple.
*)
val pp_triple :
  'a pretty_printer -> 'b pretty_printer -> 'c pretty_printer ->
  ('a * 'b * 'c) pretty_printer

(** Pretty-prints a quadruple.  The arguments are:
    - A pretty-printing function for the first element.
    - A pretty-printing function for the second element.
    - A pretty-printing function for the third element.
    - A pretty-printing function for the fourth element.
    - The formatter.
    - The quadruple.
*)
val pp_quadruple :
  'a pretty_printer -> 'b pretty_printer -> 'c pretty_printer ->
  'd pretty_printer -> ('a * 'b * 'c * 'd) pretty_printer

(** Pretty-prints a quintuple.  The arguments are:
    - A pretty-printing function for the first element.
    - A pretty-printing function for the second element.
    - A pretty-printing function for the third element.
    - A pretty-printing function for the fourth element.
    - The formatter.
    - The quintuple.
*)
val pp_quintuple :
  'a pretty_printer -> 'b pretty_printer -> 'c pretty_printer ->
  'd pretty_printer -> 'e pretty_printer ->
  ('a * 'b * 'c * 'd * 'e) pretty_printer

(** Pretty-prints a list.  The arguments are:
    - The pretty-printing function for the list.
    - The formatter to use.
    - The list.
*)
val pp_list : 'a pretty_printer -> 'a list pretty_printer

(** Pretty-prints a dictionary data structure.  The arguments are:
    - The pretty-printing function for the key.
    - The pretty-printing function for the value.
    - The enumeration function for the dictionary.
    - The formatter to use.
    - The dictionary.
*)
val pp_map : 'k pretty_printer -> 'v pretty_printer ->
  ('d -> ('k * 'v) Enum.t) -> 'd pretty_printer

(** Pretty-prints a set data structure.  The arguments are:
    - The pretty-printing function for a value.
    - The enumeration function for the set.
    - The formatter to use.
    - The dictionary.
*)
val pp_set : 'a pretty_printer -> ('s -> 'a Enum.t) -> 's pretty_printer

(** Pretty-prints an option value. *)
val pp_option : 'a pretty_printer -> 'a option pretty_printer

(** Given a pretty printer and an object, generates a string for them. *)
val pp_to_string : 'a pretty_printer -> 'a -> string

(** Suffixes a pretty printer with a fixed string. *)
val pp_suffix : 'a pretty_printer -> string -> 'a pretty_printer

(** The type of modules which give a pretty printer. *)
module type Pp =
sig
  type t
  val pp : t pretty_printer
end

(** A functor which generates a pretty printer for an existing functorized set
    module. *)
module Set_pp :
  functor(S : Set.S)(P : Pp with type t = S.elt) ->
  sig
    val pp : S.t pretty_printer
    val show : S.t -> string
  end

(** A functor which generates a pretty printer for an existing functorized
    map module. *)
module Map_pp :
  functor(M : Map.S)(P : Pp with type t = M.key) ->
  sig
    val pp : 'v pretty_printer -> 'v M.t pretty_printer
    val show : 'v pretty_printer -> 'v M.t -> string
  end
