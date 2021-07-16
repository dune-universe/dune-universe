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

(*
wrap-build-commands:
  ["%{hooks}%/sandbox.sh" "build"] {os = "linux" | os = "macos"}
wrap-install-commands:
  ["%{hooks}%/sandbox.sh" "install"] {os = "linux" | os = "macos"}
*)

let action () =
  Misc.change_opam_config (fun file_contents ->
      let file_contents =
        match CommandInstall.remove_opam_hooks file_contents with
          None -> file_contents
        | Some file_contents -> file_contents
      in
      Printf.eprintf "Restoring sandboxing hooks\n%!";
      Some (
        List.rev @@
        Misc.opam_variable "wrap-build-commands"
          "%s"
          {|
  ["%{hooks}%/sandbox.sh" "build"] {os = "linux" | os = "macos"}
|} ::
        Misc.opam_variable "wrap-install-commands"
          "%s"
          {|
  ["%{hooks}%/sandbox.sh" "build"] {os = "linux" | os = "macos"}
|} ::
        List.rev file_contents
      )
    );
  ()

let cmd = Arg.{
  cmd_name = "uninstall" ;
  cmd_action = action ;
  cmd_args = [
    [ "restore" ], Unit (fun () ->
        Misc.restore_opam_config ();
        exit 0;
      ), Ezcmd.info "Restore the previous opam config file before install/uninstall";
  ];
  cmd_man = [];
  cmd_doc = "un-install from opam config";
}
