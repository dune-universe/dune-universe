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

let cmd_name = "wrap-install"

(* We check:
   * if `_bincached/` exists, we have found a binary archive in the
     cache. We go in that directory where we should find:
     * `bin-package.version`: we should move this file to
       "%{prefix}%/etc/opam-bin/packages/%{name}%". Once the file has been
       moved, we use its absence as a marker that we shouldn't redo the
       installation
     * `bin-package.config`: optional. to be copied into
      "%{prefix}%/.opam-switch/config/%{name}%.config"
     * a directory: it is the content of the binary archive, to be copied
      into "%{prefix}%"
   * if `_binsource` exists, we are in a source archive and should exec
      the installation steps

   NOTE: we could move the installation part in a pre-install command.
 *)

let action args =
  Misc.global_log "CMD: %s\n%!"
    ( String.concat "\n    " ( cmd_name :: args) ) ;
  Misc.make_cache_dir ();
  match args with
  | name :: _version :: _depends :: cmd ->
    if Sys.file_exists ( Globals.backup_source ~name )
    || Sys.file_exists ( Globals.backup_skip ~name )
    then
      Misc.call (Array.of_list cmd)
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is '%s %s name version depends cmd...'\n%!" Globals.command cmd_name;
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
