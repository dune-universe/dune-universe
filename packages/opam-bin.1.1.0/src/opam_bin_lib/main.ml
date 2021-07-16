(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let commands = [
  CommandConfig.cmd ;
  CommandInstall.cmd ;
  CommandUninstall.cmd ;
  CommandPush.cmd ;
  CommandPull.cmd ;
  CommandClean.cmd ;
  CommandList.cmd ;
  CommandInfo.cmd ;
  CommandSearch.cmd ;

  CommandPreSession.cmd ;
  CommandPreBuild.cmd ;
  CommandWrapBuild.cmd ;
  CommandPreInstall.cmd ;
  CommandWrapInstall.cmd ;
  CommandPostInstall.cmd ;
  CommandPostSession.cmd ;
  CommandPreRemove.cmd ;
  CommandShare.cmd ;
]


let main () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [| _ ; "--version" |] ->
    Printf.printf "%s\n%!" Version.version
  | [| _ ; "--about" |] ->
    Printf.printf "%s\n%!" Globals.about
  | _ ->
(*    Misc.global_log "args: %s"
      (String.concat " " (Array.to_list Sys.argv)); *)
    try
      Ezcmd.main_with_subcommands
        ~name:Globals.command
        ~version:Version.version
        ~doc:"Create binary archives of OPAM source packages"
        ~man:[]
        commands
    with
      exn ->
      let bt = Printexc.get_backtrace () in
      let error = Printexc.to_string exn in
      Printf.eprintf "fatal exception %s\n%s\n%!" error bt ;
      Misc.global_log "fatal exception %s\n%s" error bt;
      exit 2
