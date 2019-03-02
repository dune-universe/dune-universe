open Core_kernel

(** This type is intended to be used by OCaml code to produce a visual representation
    of the diff. If you want to display this diff, take a look at [Sexp_diff_display].
*)
type t =
  | Same of Sexp.t
  | Add of Sexp.t
  | Delete of Sexp.t
  | Replace of Sexp.t * Sexp.t
  | Enclose of t list
[@@deriving sexp, hash, compare]

val invert : t -> t
val apply : t -> Sexp.t -> Sexp.t Or_error.t
val apply_exn : t -> Sexp.t -> Sexp.t
val print_for_test : t -> unit
