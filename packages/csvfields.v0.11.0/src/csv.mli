open! Core

module Helper : sig

  (** Prepend the name of a field to the list of fields. This function is Fields.fold
  friendly.  *)
  val prepend_name :
    string list
    -> (_, _) Field.t
    -> string list

  (** Add the two integer parameters. This function is Fields.fold friendly *)
  val add :
    int
    -> int
    -> _
    -> int

  (** [write ~is_first ~is_last ~writer ~to_string _ _ t] writes the contents of a field
  using [writer]. If [not is_first] [writer ","; writer (to_string t)]. If [is_last] then
  [writer (to_string t); writer "\n"]. *)
  val write :
    is_first:bool
    -> is_last:bool
    -> writer:(string -> unit)
    -> to_string:('a -> string)
    -> _
    -> _
    -> 'a
    -> unit

end

module Spec : sig
  (* A tree describing the header of a record in a csv. A field [x] becomes [Leaf x] if it
  is an atom. Otherwise, if a record, it becomes a tree with children generated
  recursively. *)
  type t = Leaf of string | Tree of (string * t list)

  val depth : t list -> int

  (** A csv represention of a tree. *)
  val header : t list -> string list list

  val matches : string list list -> t list -> bool
  val check : csv:string list list -> header:t list -> f:(string list list -> 'a) -> 'a
end

module type Csvable_simple = sig
  (** The [t] that can be represented in a CSV file. *)
  type t

  (** [true] if [csvable] will be represented as a column, and its label is the field name
  of a containing record. *)
  val is_csv_atom : bool

  (** A [Fields.fold] friendly version of a function to collect the csv header of the
  [csvable] type. The output is the header in reverse order. *)
  val rev_csv_header' : string list -> _ -> _ -> string list
  val rev_csv_header_spec' : Spec.t list -> _ -> _ -> Spec.t list

  (** [t_of_row' _ row] generates a [Fields.make_creator] friendly function that outputs a
  pair [(creator, tail)] such that [creator ()] generates the type [t] with the first
  elements in [row], and [tail] is the remaining set of elements from [row] that where not
  used in the creation of [t]. *)
  val t_of_row' : _ -> string list -> (unit -> t) * (string list)

  (** [row_of_t'] has the same arguments as [Helper.write]. The ignored arguments are
  used to make it [Fields.fold] friendly. *)
  val write_row_of_t' :
    is_first:bool
    -> is_last:bool
    -> writer:(string -> unit)
    -> _
    -> _
    -> t
    -> unit
end

exception Excess_of_elements_in_row of string list
exception Illegal_atom of string

module type Csvable = sig
  include Csvable_simple

  (** The list of strings creating the header of the [csvable] type. *)
  val csv_header : string list
  val csv_header_spec : Spec.t list
  (** [t_of_row row] creates type [t] from the [row]. *)
  val t_of_row : string list -> t

  (** [row_of_t t] creates a [row] from [csvable]. *)
  val row_of_t : t -> string list

  (** The following functions are wrappers around the corresponding functions in
    [Csvlib.Csv]. *)
  val csv_load : ?separator:char -> string -> t list
  val csv_load_in : ?separator:char -> In_channel.t -> t list
  val csv_save_fn : ?separator:char -> (string -> unit) -> t list -> unit
  val csv_save_out : ?separator:char -> Out_channel.t -> t list -> unit
  val csv_save : ?separator:char -> string -> t list -> unit

end

module type Stringable = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end

(** A functor to quickly generate a Csvable module using its string conversion
functions *)
module Atom (S : Stringable) : Csvable with type t = S.t

(** All the conversion functions for internal use *)
module Make_csvable_simple (S : Stringable) : Csvable_simple with type t := S.t

module Record (S : Csvable_simple) : Csvable with type t = S.t

val unit_of_row  : _ -> string list -> (_ -> unit) * string list

val bool_of_row : _ -> string list -> (_ -> bool) * string list

val string_of_row : _ -> string list -> (_ -> string) * string list

val char_of_row : _ -> string list -> (_ -> char) * string list

val int_of_row : _ -> string list -> (_ -> int) * string list

val float_of_row : _ -> string list -> (_ -> float) * string list

val int32_of_row : _ -> string list -> (_ -> int32) * string list

val int64_of_row : _ -> string list -> (_ -> int64) * string list

val nativeint_of_row : _ -> string list -> (_ -> nativeint) * string list

val big_int_of_row : _ -> string list -> (_ -> Big_int.big_int) * string list

val nat_of_row : _ -> string list -> (_ -> Nat.nat) * string list

val num_of_row : _ -> string list -> (_ -> Num.num) * string list

val ratio_of_row : _ -> string list -> (_ -> Ratio.ratio) * string list

type ('a, 'b, 'c) row_of =
  is_first:bool
  -> is_last:bool
  -> writer:(string -> unit)
  -> 'b
  -> 'c
  -> 'a
  -> unit

val row_of_unit : (unit, _, _) row_of

val row_of_bool : (bool, _, _) row_of

val row_of_string : (string, _, _) row_of

val row_of_char : (char, _, _) row_of

val row_of_int : (int, _, _) row_of

val row_of_float : (float, _, _) row_of

val row_of_int32 : (int32, _, _) row_of

val row_of_int64 : (int64, _, _) row_of

val row_of_nativeint : (nativeint, _, _) row_of

val row_of_big_int : (Big_int.big_int, _, _) row_of

val row_of_nat : (Nat.nat, _, _) row_of

val row_of_num : (Num.num, _, _) row_of

val row_of_ratio : (Ratio.ratio, _, _) row_of
