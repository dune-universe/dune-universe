open Sexplib

(**
   The [load...] functions of this module mirror the corresponding functions of
   the [Sexp] module except that they do macro-expansion in the loaded file and
   may throw additional exceptions.
*)

(** [load_sexp file] like [{!Sexp.load_sexp} file], but resolves the macros
    contained in [file].

    If [allow_includes] is [false] then this raises an exception if [file] contains any
    :include macros. The default is [true]. *)
val load_sexp : ?allow_includes:bool -> string -> Sexp.t

(** [load_sexps file] like [{!Sexp.load_sexps} file], but resolves the macros
    contained in [file].

    If [allow_includes] is [false] then this raises an exception if [file] contains any
    :include macros. The default is [true]. *)
val load_sexps : ?allow_includes:bool -> string -> Sexp.t list

(** [load_sexp_conv file f] uses {!load_sexp} and converts the result using
    [f]. *)
val load_sexp_conv
  :  ?allow_includes:bool
  -> string
  -> (Sexp.t -> 'a)
  -> 'a Macro.annot_conv

(** [load_sexps_conv file f] uses {!load_sexps} and converts the result using
    [f]. *)
val load_sexps_conv
  :  ?allow_includes:bool
  -> string
  -> (Sexp.t -> 'a)
  -> 'a Macro.annot_conv list

(** [load_sexp_conv_exn file f] like {!load_sexp_conv}, but raises an exception
    in case of conversion error. *)
val load_sexp_conv_exn : ?allow_includes:bool -> string -> (Sexp.t -> 'a) -> 'a

(** [load_sexps_conv_exn file f] like {!load_sexps_conv}, but raises an
    exception in case of conversion error. *)
val load_sexps_conv_exn : ?allow_includes:bool -> string -> (Sexp.t -> 'a) -> 'a list

(** [included_files] returns the names of all files that will be loaded as result of macro
    expansion. This includes the file itself. *)
val included_files : string -> string list
