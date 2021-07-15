(* Time-stamp: <modified the 11/12/2012 (at 16:34) by Erwan Jahier> *)

(** Define a function missing in the Filename ocaml stdlib *)


(** Simplify the path of a file name.

   For instance, in posix, "./x/../file.ext" is simplified into  "./file.ext"
*)
val simplify : string -> string
