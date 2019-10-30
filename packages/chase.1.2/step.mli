(** Chase step *)

open Formula
open Structure

(** Given a structure and a formula, the step function takes a chase
   step.  It returns None if the structure satisfies the formula.
   Otherwise it returns a list of structures that have been extended
   so as to satisfy the formula. *)
val step : structure -> form -> structure list option
