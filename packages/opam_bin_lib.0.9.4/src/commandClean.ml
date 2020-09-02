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

let cmd_name = "clean"

let clean_log () =
  Sys.remove Globals.opambin_log ;
  ()

let clean_store () =
  List.iter (fun dir ->
      Printf.eprintf "Cleaning %s\n%!" dir;
      Misc.call [| "rm"; "-rf" ; dir |];
      EzFile.make_dir ~p:true dir ;
    )
    [ Globals.opambin_cache_dir ;
      Globals.opambin_store_repo_packages_dir ;
      Globals.opambin_store_archives_dir ;
    ];
  let store_dir = Globals.opambin_store_dir in
  let files = Sys.readdir store_dir in
  Array.iter (fun file ->
      let packages_dir = store_dir // file // "packages" in
      if Sys.file_exists packages_dir then begin
        Printf.eprintf "Cleaning %s\n%!" packages_dir;
        Misc.call [| "rm" ; "-rf"; packages_dir |];
      end
    ) files;
  Misc.call [| "opam"; "update" |];
  ()

let clean_all () =
  clean_log ();
  clean_store ();
  (* flush the copy of the repo that opam keeps *)
  ()

let action args =
  match args with
  | [] -> clean_all ()
  | _ ->
    List.iter (function
        | "all" -> clean_all ()
        | "log" -> clean_log ()
        | "store" -> clean_store ()
        | s ->
          Printf.eprintf "Unexpected argument %S.\n%!" s;
          exit 2) args

let cmd =
  let anon_args = ref [] in
  {
  cmd_name ;
  cmd_action = (fun () -> action !anon_args) ;
  cmd_args = [
    [], Arg.Anons (fun list -> anon_args := list),
    Ezcmd.info "What to clean (`all`, `log` or `store`)";
  ];
  cmd_man = [];
  cmd_doc = "clear all packages and archives from the cache and store";
}
