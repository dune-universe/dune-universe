(** [Re2_stable] adds an incomplete but stable serialization of [Re2].

    Feel free to extend it as necessary. *)

open! Core

(** This serialization only transmits the pattern, not the options. *)
module V1 : Stable_without_comparator with type t = Re2.t
