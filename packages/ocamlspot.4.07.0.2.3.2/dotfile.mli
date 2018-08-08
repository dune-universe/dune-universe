(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* module for .ocamlspot file 

   build_dir=dirname

      Work around for build systems which create object files to exotic places.
      If .ocamlspot is placed in a directory $DIR,
      then .cmt* files of source files under $DIR ex. $DIR/subdir/source.ml
      is searched in $DIR/dirname/subdir/subdir/.
*)

val find : 
  dir: string
  -> name: string
  -> ( int      (*+ how many dir levels up *)
       * string (*+ the postfix *)
       * string (*+ the directory which has the file *)
       * string (*+ the path of the file found *)
     ) option
(** Find a dot file in parent directories. *)

val load_raw : string -> (string, string option) Hashtbl.t
(** Load the specified dot file *)
   
type t = {
  dir_level : int;
  build_dir : string option;
  module_prefix : string option;
}

val load : int (*+ dir level *)
    -> string 
    -> t

val find_and_load : string (* dir *) -> (string * string * t) option
  (** [find_and_load abspath] searches .ocamlspot file 
      and returns its location (directory) and its contents.
      The search starts from directory [abspath] and if not found
      it checks the parent directory recursively till the root. *)
