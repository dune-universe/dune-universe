open Sexplib

(**
   The [load...] functions of this module mirror the corresponding functions of
   the [Sexp] module except that they do macro-expansion in the loaded file and
   may throw additional exceptions.
*)

(** [load_sexp file] like [{!Sexp.load_sexp} file], but resolves the macros
    contained in [file]. *)
val load_sexp : string -> Sexp.t

(** [load_sexps file] like [{!Sexp.load_sexps} file], but resolves the macros
    contained in [file]. *)
val load_sexps : string -> Sexp.t list

(** [load_sexp_conv file f] uses {!load_sexp} and converts the result using
    [f]. *)
val load_sexp_conv : string -> (Sexp.t -> 'a) -> 'a Macro.annot_conv

(** [load_sexps_conv file f] uses {!load_sexps} and converts the result using
    [f]. *)
val load_sexps_conv : string -> (Sexp.t -> 'a) -> 'a Macro.annot_conv list

(** [load_sexp_conv_exn file f] like {!load_sexp_conv}, but raises an exception
    in case of conversion error. *)
val load_sexp_conv_exn : string -> (Sexp.t -> 'a) -> 'a

(** [load_sexps_conv_exn file f] like {!load_sexps_conv}, but raises an
    exception in case of conversion error. *)
val load_sexps_conv_exn : string -> (Sexp.t -> 'a) -> 'a list
