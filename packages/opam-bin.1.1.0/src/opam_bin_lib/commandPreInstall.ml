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

let cmd_name = "pre-install"

(* We check:
   * if `_bincached/` exists, we have found a binary archive in the
     cache. We go in that directory where we should find:
     * `bin-package.version`: we should move this file to
       "%{prefix}%/etc/opam-bin/packages/%{name}%". Once the file has been
       moved, we use its absence as a marker that we shouldn't redo the
       installation
     * `bin-package.config`: optional. to be copied into
      "%{prefix}%/.opam-switch/config/%{name}%.config"
     * a directory: it is the content of the binary archive, to be copied
      into "%{prefix}%"
   * if `_binsource` exists, we are in a source archive and should exec
      the installation steps

   NOTE: we could move the installation part in a pre-install command.
 *)

(* During the pre-install, we should copy local markers to switch
   markers, to avoid them being copied by install steps. We could not
   have done it before because they would be removed by the removal of
   a previous version of the package happening between build and
   install. *)

let action args =
  Misc.log_cmd cmd_name args ;
  Misc.make_cache_dir ();
  match args with
  | name :: _version :: _depends :: [] ->
    let marker_dir = Globals.opambin_switch_temp_dir () in
    if not ( Sys.file_exists marker_dir ) then
      EzFile.make_dir marker_dir;
    List.iter (fun (marker, backup) ->
        if Sys.file_exists marker then
          Sys.rename marker ( backup ~name )
      ) [
      Globals.marker_skip , Globals.backup_skip;
      Globals.marker_source , Globals.backup_source;
      Globals.marker_opam , Globals.backup_opam;
      Globals.marker_patch ,Globals.backup_patch;
    ];

    if Sys.file_exists ( Globals.backup_source ~name )
    || Sys.file_exists ( Globals.backup_skip ~name )
    then
      ()
    else
    if Sys.file_exists Globals.marker_cached then begin
      Unix.chdir Globals.marker_cached;
      if Sys.file_exists Globals.package_version then
        let files = Sys.readdir "." in
        Array.iter (fun file ->
            if file = Globals.package_version then begin
              let packages_dir =
                Globals.opambin_switch_packages_dir () in
              EzFile.make_dir ~p:true packages_dir;
              Sys.rename file ( packages_dir // name )
            end
            else
            if file =  Globals.package_config then begin
              let config_dir = Globals.opam_switch_internal_config_dir
                  () in
              EzFile.make_dir ~p:true config_dir ;
              Sys.rename file ( config_dir // Printf.sprintf "%s.config" name )
            end else
            if file = Globals.package_info then
              Sys.remove Globals.package_info
            else
              let pwd = Unix.getcwd () in
              Unix.chdir file ;
              Misc.call [| "cp" ; "-aT" ; "." ;
                                  Globals.opam_switch_dir () |];
              Unix.chdir pwd
          ) files
    end
  | _ ->
    Printf.eprintf
      "Unexpected args: usage is '%s %s name version depends cmd...'\n%!" Globals.command cmd_name;
    exit 2

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
  cmd_doc = "(opam hook) Exec or not build commands";
}
