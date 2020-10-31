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

let cmd_name = "list"

let action () =
  Printf.eprintf "Binary packages in %s:\n%!"
    Globals.opambin_store_archives_dir;
  EzFile.make_dir ~p:true Globals.opambin_store_archives_dir ;
  Misc.call [| "ls" ; Globals.opambin_store_archives_dir |]

let cmd = {
  cmd_name ;
  cmd_action = action ;
  cmd_args = [];
  cmd_man = [];
  cmd_doc = "List binary packages created on this computer";
}
