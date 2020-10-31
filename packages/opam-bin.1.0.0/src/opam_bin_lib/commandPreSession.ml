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
open EzConfig.OP
(* open EzFile.OP *)

let cmd_name = "pre-session"

let action args =
  Misc.log_cmd cmd_name args ;
  Misc.global_log "Session started";
  begin
    if !!Config.enabled then
      if Misc.not_this_switch () then
        Printf.printf "%s disabled in this switch\n%!" Globals.command
      else
        Printf.printf "%s [ creation: %b, sharing: %b ]\n%!" Globals.command
          !!Config.create_enabled !!Config.share_enabled
    else
      Printf.printf "%s disabled\n%!" Globals.command
  end;
  let filename = Globals.opambin_session_msg_file () in
  if Sys.file_exists filename then
    Sys.remove filename
  else
    EzFile.make_dir ~p:true (Filename.dirname filename)

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
  cmd_doc = "(opam hook) Start Session";
}
