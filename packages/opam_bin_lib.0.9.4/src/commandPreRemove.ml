(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.TYPES
open EzFile.OP

let cmd_name = "pre-remove"

let action args =
  Misc.global_log "CMD: %s"
    ( String.concat "\n    " ( cmd_name :: args) ) ;
  match args with
  | name :: _version :: _depends :: [] ->
    List.iter (fun file_name ->
        try
          Sys.remove file_name
        with _ -> ()
      ) [
      Globals.opambin_switch_packages_dir () // name ;
      Globals.backup_marker ~name ".source" ;
      Globals.backup_marker ~name ".skip" ;
      Globals.backup_marker ~name ".opam" ;
      Globals.backup_marker ~name ".patch" ;
    ]
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is '%s pre-remove name version uid depends'\n%!"
      Globals.command ;
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
  cmd_doc = "(opam hook) Remove binary install artefacts";
}
