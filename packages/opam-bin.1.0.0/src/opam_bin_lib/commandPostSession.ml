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
(* open EzFile.OP *)

let cmd_name = "post-session"

let action args =
  Misc.log_cmd cmd_name args ;
  Misc.global_log "Session ended";
  let filename = Globals.opambin_session_msg_file () in
  if Sys.file_exists filename then
    let s = EzFile.read_file filename in
    Printf.printf "%s actions:\n%s%!" Globals.command s;
    Sys.remove filename
      (*
  else
    Printf.printf "File %s does not exist\n%!" filename
*)

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
  cmd_doc = "(opam hook) End Session";
}
