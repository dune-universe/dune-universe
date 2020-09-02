(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ezcmd.TYPES

(*
TODO:
* Check if the archive already exists
  * if 'bin-package.version' exists, we are in a binary archive,
      execute all commands.
  * otherwise, check if a binary archive exists.
     If no, create an empty
       `bin-package.version` file to force execution of all commands.
     If yes, create a directory package.cached/ containing that archive,
       bin-package.version and bin-package.config.
     In wrap-install, perform the installation of the archive.
*)

let cmd_name = "wrap-build"

let action args =
  Misc.global_log "CMD: %s\n%!"
    ( String.concat "\n    " ( cmd_name :: args) ) ;
  Misc.make_cache_dir ();
  match args with
  | _name :: _version :: _depends :: cmd ->
    if Sys.file_exists Globals.marker_source
    || Sys.file_exists Globals.marker_skip
    then
      Misc.call (Array.of_list cmd)
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is '%s %s name version depends cmd...'\n%!" Globals.command cmd_name ;
    exit 2

let cmd =
  let args = ref [] in
  Arg.{
  cmd_name ;
  cmd_action = (fun () -> action !args) ;
  cmd_args = [
    [], Anons (fun list -> args := list),
    Ezcmd.info "args"
  ];
  cmd_man = [];
  cmd_doc = "(opam hook) Exec or not build commands";
}
