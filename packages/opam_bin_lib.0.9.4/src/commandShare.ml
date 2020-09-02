(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(*
open EzCompat
open EzFile.OP
open EzConfig.OP
open OpamParserTypes
*)

open Ezcmd.TYPES

let cmd_name = "share"

let cmd =
  let args = ref [] in
  let recursive = ref false in
  let share_dir = ref None in
  Arg.{
    cmd_name ;
    cmd_action = (fun () ->
        let args =
          if !recursive then
            let files = ref [] in
            let rec iter file =
              match Unix.lstat file with
              | exception _ -> ()
              | st ->
                match st.st_kind with
                | Unix.S_REG -> files := file :: !files
                | S_DIR ->
                  begin
                    (* Since opam-bin share will be used on .opam and switches,
                       we should hardcore some directory exclusion... *)
                    match Filename.basename file with
                    | ".opam-switch"
                    | "repo"
                    | "plugins"
                    | "download-cache"
                    | "log"
                    | "var" -> ()
                    | _ ->
                      EzFile.iter_dir (fun ~basename:_ ~localpath:_ ~file  ->
                          iter file
                        ) file
                  end
                | _ -> ()
            in
            List.iter iter ( List.rev !args );
            !files
          else !args
        in
        let share_dir = !share_dir in
        Share.files ?share_dir args) ;
    cmd_args = [
      [ "r" ; "rec" ], Arg.Set recursive,
      Ezcmd.info "Iter on directories";

      [ "share-dir" ], Arg.String (fun dir -> share_dir := Some dir),
      Ezcmd.info "Global directory of shared files" ;

      [], Anons (fun list -> args := list),
      Ezcmd.info "args"
    ];
    cmd_man = [];
    cmd_doc = "Share the following files";
  }
