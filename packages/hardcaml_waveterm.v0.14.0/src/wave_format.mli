(** Display format for a single wave (a row in the complete waveform). *)

open! Import

type alignment =
  | Left
  | Right
[@@deriving sexp_of]

type t =
  | Binary (** Binary. *)
  | Bit (** Like [Binary], but shows a single bit as a line. *)
  | Bit_or of t (** If [width=1] then [Bits] otherwise [t] *)
  | Hex (** Hexadecimal. *)
  | Unsigned_int (** Unsigned integer. *)
  | Int (** Signed integer. *)
  | Index of string list (** Use data as index into given list of strings. *)
  | Custom of (Bits.t -> string) (** User defined formatting. *)
[@@deriving sexp_of]

(** [Custom] constructors are compared for physical equality. *)
include
  Equal.S with type t := t
