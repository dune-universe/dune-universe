(** Helpers for using topkg with jbuilder.

    {e 0.2.0 - {{:https://github.com/samoht/topkg-jbuilder}homepage}} *)

open Topkg

(** Similar to [Topkg.Pkg.describe] but with different defaults suited for jbuilder.

    You need to specify [name] when you have multiple `<package>.opam` files and they are
    not all prefixed by the shortest package name.
*)
val describe
  :  ?delegate:Cmd.t
  -> ?readmes:Pkg.std_file list
  -> ?licenses:Pkg.std_file list
  -> ?change_logs:Pkg.std_file list
  -> ?lint_files:fpath list option
  -> ?lint_custom:(unit -> R.msg result list)
  -> ?distrib:Pkg.distrib
  -> ?publish:Pkg.publish
  -> ?name:string
  -> unit
  -> unit


