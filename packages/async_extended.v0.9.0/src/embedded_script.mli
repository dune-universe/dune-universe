(* Extraction and running of scripts embedded inside an executable. *)

open! Core
open! Async

val run : script_name:string
  -> args:string list
  -> see_output:bool
  -> [ `Ok | `Fail ] Deferred.t

