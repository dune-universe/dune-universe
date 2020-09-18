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
open EzFile.OP

let cmd_name = "pull"

let action () =

  match !!Config.rsync_url with
  | None ->
    Printf.eprintf
      "Error: you must define the remote url with `%s config --rsync-url`\n%!"
      Globals.command ;
    exit 2
  | Some rsync_url ->

    let args = [ "rsync"; "-auv" ; "--progress" ] in
    let args = args @ [
        rsync_url // ".";
        Globals.opambin_store_dir // "." ;
      ] in
    Printf.eprintf "Calling '%s'\n%!"
      (String.concat " " args);
    Misc.call (Array.of_list args);
    Printf.eprintf "Done.\n%!";
    ()

let cmd =
  {
    cmd_name ;
    cmd_action = (fun () -> action () ) ;
    cmd_args = [] ;
    cmd_man = [];
    cmd_doc = "push binary packages to the remote server";
  }
