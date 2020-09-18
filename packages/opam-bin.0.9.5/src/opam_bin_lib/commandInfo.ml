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

let cmd_name = "info"

let action tail =
  Printf.eprintf "Info on binary packages:\n%!";
  if tail then begin
    if not ( Sys.file_exists Globals.opambin_info ) then
      Misc.call [| "touch" ; Globals.opambin_info |] ;
    Misc.call [| "tail"; "-f" ; Globals.opambin_info |]
  end else
  if Sys.file_exists Globals.opambin_info then
    Misc.call [| "cat" ; Globals.opambin_info |]

let cmd =
  let tail = ref false in
  {
    cmd_name ;
    cmd_action = (fun () -> action !tail) ;
    cmd_args = [
      [ "tail" ], Arg.Set tail,
      Ezcmd.info "Use tail -f to watch";

    ];
    cmd_man = [];
    cmd_doc = "Display information on binary packages";
  }
