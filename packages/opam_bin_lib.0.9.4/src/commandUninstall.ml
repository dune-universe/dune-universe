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

let remove_opam_hooks file_contents =
  let rec iter items found rev =
    match items with
    | [] ->
      if found then begin
        Printf.eprintf "Found hooks to remove\n%!";
        Some ( List.rev rev )
      end
      else begin
        Printf.eprintf "No hooks to remove\n%!";
        None
      end
    | item :: items ->
      match item with
      | OpamParserTypes.Variable (_, name, _) ->
        begin
          match name with
          | "pre-build-commands"
          | "wrap-build-commands"
          | "pre-install-commands"
          | "wrap-install-commands"
          | "post-install-commands"
          | "pre-remove-commands"
            -> iter items true rev
          | _ -> iter items found ( item :: rev )
        end
      | _ ->
        iter items found ( item :: rev )
  in
  iter file_contents false []

let action () =
  Misc.change_opam_config (fun file_contents ->
      let file_contents = match remove_opam_hooks file_contents with
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
