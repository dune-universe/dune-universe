(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Ezcmd.TYPES
open EzFile.OP
(*
open EzConfig.OP
open OpamParserTypes
*)

let cmd_name = "search"

let iter f =
  let tested_set = ref StringSet.empty in
  Misc.iter_repos ~cont:ignore
    (  Misc.all_repos () )
    (fun ~repo ~package ~version ->
      (* Misc.global_log "searching %s" version; *)
      if StringSet.mem version !tested_set then
        false
      else
        let file =
          ( repo // "packages" // package // version // "files" //
            Globals.package_info )
        in
        tested_set := StringSet.add version !tested_set ;
        (*      Printf.eprintf "     Searching %s\n%!" file ; *)
        try
          if Sys.file_exists file then
            EzFile.iter_lines (fun line ->
                if f line then
                  Printf.printf "%s:%s\n%!" version line
              )  file ;
          false
        with exn ->
          Printf.eprintf "warning: exception %s while searching %s\n%!"
            (Printexc.to_string exn) file;
          false
    )

let action ~i ?anon_arg () =
  let find_regexp core =
    let core = if i then Re.no_case core else core in
    let re = Re.compile core in
    iter (fun line ->
        Re.execp re line
      )
  in
  match anon_arg with
  | None -> ()
  | Some s ->
    find_regexp ( Re.str s )

let cmd =
  let anon_arg = ref None in
  let i = ref false in
  Arg.{
    cmd_name ;
    cmd_action = (fun () ->
        action
          ~i:!i
          ?anon_arg:!anon_arg
          ()) ;
    cmd_args = [
      ["i"], Arg.Set i,
      Ezcmd.info "Case-insensitive matching" ;

      [], Anon (0, fun s -> anon_arg := Some s),
      Ezcmd.info "Search string" ;
    ];
    cmd_man = [];
    cmd_doc = "Search binary packages";
  }
