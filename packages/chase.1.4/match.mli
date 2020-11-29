(** Match a conjunction *)

open Prelude
open Sym
open Formula
open Structure

(** An environment maps a symbol standing for a variable to a
   symbol standing for a constant. *)
type env = (sym * sym) list

(** Match a conjunction of atomic formulas with a structure.  The
   result is a stream of environments. *)
val match_conj : structure -> conj -> env stream
