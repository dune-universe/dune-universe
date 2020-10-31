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
open EzConfig.OP
open OpamParserTypes

let cmd_name = "pre-build"

let cache_file ~cache ~md5 =
  cache // "md5" // String.sub md5 0 2 // md5

let check_cache_file ~cache ~md5 =
  let file = cache_file ~cache ~md5 in
  if Sys.file_exists file then Some file else None

let find_archive_in_cache ~repo ~md5 =
  match check_cache_file ~cache:Globals.opam_cache_dir ~md5 with
  | Some file -> Some file
  | None ->
    match check_cache_file ~cache:Globals.opambin_cache_dir ~md5 with
    | Some file -> Some file
    | None ->
      ignore repo;
    (* TODO: lookup repo specific caches ?
(* We should read .opam/repos-config to get the URL of the repository:
```
repositories: [
  "default" {"file:///home/---/GIT/opam-repository-relocatable"}
  "local-bin" {"file:///home/---/.opam/opam-bin/store/repo"}
]
```
together with .opam/repo/XXX/repo:
```
opam-version: "2.0"
archive-mirrors: "../../cache"
```
 *)
    match check_cache_file ~cache:(repo // "cache") ~md5 with
    | Some file -> Some file
    | None ->
      match OpamParser.file (repo // "repo") with
      | exception _ -> None
      | Some opam ->
        let cache = ref None in
        List.iter (function
            | Variable ( _, "archive-mirrors" , String ( _, v ) ) ->
              cache := v
          ) opam.file_contents ;
        match !cache with
        | None -> None
        | Some cache ->
*)
      None

let check_cached_binary_archive ~version ~repo ~package =
  Misc.global_log "found binary package in repo %s" repo;
  let package_dir = repo // "packages" // package // version in
  let src = ref None in
  let md5 = ref None in
  let opam = OpamParser.file ( package_dir // "opam" ) in
  List.iter (function
      | Section ( _ , { section_kind = "url" ; section_items ; _ } ) ->
        List.iter (function
              Variable ( _, "src", String ( _ , v )) -> src := Some v
            | Variable ( _, "checksum",
                         List ( _, [ String ( _, v ) ] )) ->
              assert ( EzString.starts_with v ~prefix:"md5=" );
              let len = String.length v in
              md5 := Some ( String.sub v 4 (len-4) )
            | _ -> ()
          ) section_items
      | _ -> ()
    ) opam.file_contents ;
  let binary_archive =
    match !md5 with
    | None ->
      Misc.global_log "url.checksum.md5 not found";
      None
    | Some md5 ->
      match find_archive_in_cache ~repo ~md5 with
      | Some binary_archive ->
        Some binary_archive
      | None ->
        match !src with
        | None ->
          Printf.eprintf "error: url.src not found\n%!";
          exit 2
        | Some url ->

          let temp_dir = Globals.opambin_switch_temp_dir () in
          let output = temp_dir // md5 in
          match Misc.wget ~url ~output with
          | None ->
            Printf.eprintf "Error: could not download archive at %S\n%!" url;
            exit 2
          | Some binary_archive ->
            let digest = Digest.file binary_archive in
            assert ( Digest.to_hex digest = md5 );
            let cache_dir =
              Globals.opam_cache_dir //
              "md5" // String.sub md5 0 2 in
            let cached_file = cache_dir // md5 in
            EzFile.make_dir ~p:true cache_dir;
            Sys.rename binary_archive cached_file ;
            Some cached_file
  in
  EzFile.make_dir Globals.marker_cached ;
  let install_file = package ^ ".install" in
  if Sys.file_exists install_file then
    Sys.remove install_file ;
  Unix.chdir Globals.marker_cached ;
  let package_files = package_dir // "files" in
  let s = EzFile.read_file
      ( package_files // Globals.package_version ) in
  EzFile.write_file Globals.package_version s ;
  begin
    match EzFile.read_file
            ( package_files // Globals.package_config ) with
    | exception _ -> ()
    | s ->
      EzFile.write_file Globals.package_config s
  end;
  begin
    match binary_archive with
    | None -> ()
    | Some binary_archive ->
      Misc.call [| "tar" ; "zxf" ; binary_archive |] ;
  end;
  true

let chop_prefix s ~prefix =
  if EzString.starts_with s ~prefix then
    let prefix_len = String.length prefix in
    let len = String.length s in
    Some ( String.sub s prefix_len (len - prefix_len) )
  else
    None

let chop_suffix s ~suffix =
  if EzString.ends_with s ~suffix then
    let suffix_len = String.length suffix in
    let len = String.length s in
    Some ( String.sub s 0 (len - suffix_len) )
  else
    None

let has_equal_suffix v =
  let len = String.length v in
  assert ( len > 0 );
  v.[len-1] = '='

let maybe_apply_patch ~name ~version =
  let keep_version = version in
  let patches_dir =
    let patches_url = !!Config.patches_url in
    match chop_prefix patches_url ~prefix:"file://" with
    | Some s -> s
    | None -> Globals.opambin_patches_dir
  in
  if not ( Sys.file_exists patches_dir ) then
    Printf.kprintf failwith
      {|
Error: patches dir '%s' does not exist.\n
  Maybe you didn't use 'opam-bin install patches' ?\n%!|}
      patches_dir ;
  let rec iter_package package =
    let package_dir = patches_dir // "patches" // package in
    if Sys.file_exists package_dir then
      let files = Sys.readdir package_dir in
      Misc.global_log "package %s needs relocation" name;
      let versions = ref [] in
      let alias = ref None in
      Array.iter (fun file ->
          match chop_suffix file ~suffix:".alias" with
          | Some package -> alias := Some package
          | None ->
            match chop_suffix file ~suffix:".patch" with
            | Some version -> versions := version :: !versions
            | None -> ()
        ) files;
      match !alias with
      | Some package ->
          Misc.global_log "lookup patches for %s instead" package;
          iter_package package
      | None ->
        let versions = Array.of_list !versions in
        Array.sort VersionCompare.compare versions ;
        let rec iter version versions current =
          match versions with
          | [] -> current
          | v :: versions ->
            if has_equal_suffix v then
              if v = version ^ "=" then
                Some v
              else
                iter version versions current
            else
            if VersionCompare.compare version v >= 0 then
              iter version versions (Some v)
            else current
        in
        match iter version (Array.to_list versions) None with
        | None ->
          Misc.global_log_err
            "Package %S is not relocatable, but no patch found for version %S.\n%!"
            name version;
          Misc.global_log_err
            "You may have to disable opam-bin to install that version.\n%!";
          false
        | Some version ->
          let patch = package_dir // version ^ ".patch" in
          Misc.global_log "Using patch %s for %s.%s"
            patch name keep_version ;
          Misc.call [| "cp" ; "-f";
                              patch ; Globals.marker_patch |];
          Misc.call [| "patch" ; "-p1"; "-i"; patch |] ;
          if Sys.file_exists "reloc-patch.sh" then begin
            Misc.call [| "sh"; "./reloc-patch.sh" |];
          end;
          true
    else
      true
  in
  iter_package name

let cached_binary_archive ~name ~version ~depends =
  if not ( maybe_apply_patch ~name ~version ) then
    `NotRelocatable
  else
    let ( source_md5, _depends, _dependset, missing_versions, _opam_file ) =
      CommandPostInstall.compute_hash
        ~name ~version ~depends () in
    if missing_versions <> [] then
      `MissingVersions missing_versions
    else
      let version_prefix = Printf.sprintf "%s.%s+bin+%s+"
          name version source_md5 in
      if Misc.iter_repos ~cont:(fun x -> x)
          ( Misc.all_repos () )
          (fun ~repo ~package ~version ->
          if EzString.starts_with version ~prefix:version_prefix then begin
            check_cached_binary_archive ~package ~repo ~version
          end else
            false
        ) then
        `BinaryArchiveFound
      else begin
        Misc.global_log "Could not find cached binary package %s"
          version_prefix ;
        `NoBinaryArchiveFound source_md5
      end

let error_on_compile =
  match Sys.getenv "OPAM_BIN_FORCE" with
  | exception _ -> false
  | _ -> true

let error_on_non_reloc =
  match Sys.getenv "OPAM_BIN_RELOC" with
  | exception _ -> false
  | _ -> true

let action args =
  Misc.log_cmd cmd_name args ;
  match args with
  | name :: version :: depends :: [] ->
    let marker_skip = Globals.marker_skip in
    if not !!Config.enabled
    || Misc.not_this_switch () then begin
      Misc.global_log "opam-bin is disabled";
      EzFile.write_file marker_skip
        "opam-bin is disabled";
    end else
      let marker_source = Globals.marker_source in
      let marker_opam = Globals.marker_opam in
      let marker_patch = Globals.marker_patch in
      if Sys.file_exists marker_source then begin
        Misc.global_log "removing marker_source";
        Sys.remove marker_source ;
      end;
      if Sys.file_exists marker_opam then begin
        Misc.global_log "removing marker_opam";
        Sys.remove marker_opam ;
      end;
      if Sys.file_exists marker_patch then begin
        Misc.global_log "removing marker_patch";
        Sys.remove marker_patch ;
      end;
      if Sys.file_exists Globals.marker_cached then begin
        Misc.global_log "%s should not already exist!"
          Globals.marker_cached;
        exit 2
      end else
      if Sys.file_exists Globals.package_version then begin
        Misc.global_log "already a binary package";
        EzFile.write_file marker_source "already-a-binary-package";
      end else begin
        Misc.global_log "checking for cached archive";
        match cached_binary_archive ~name ~version ~depends with
        | `BinaryArchiveFound ->
          Misc.global_log "found a binary archive in cache";
          (* this should have created a marker_cached/ directory *)
        | `NoBinaryArchiveFound source_md5 ->
          Misc.global_log "no binary archive found.";
          if error_on_compile then begin
            Printf.eprintf
              "Error: opam-bin is configured to prevent compilation.\n%!";
            exit 2
          end;
          EzFile.write_file marker_source source_md5
        | `MissingVersions missing_versions ->
          EzFile.write_file marker_skip
            ( Printf.sprintf "Missing binary deps: %s"
                ( String.concat " " missing_versions ) )
        | `NotRelocatable ->
          if error_on_non_reloc then begin
            Printf.eprintf
              "Error: opam-bin is configured to force relocation.\n%!";
            exit 2
          end;
          EzFile.write_file marker_skip
            "Missing relocation patch for unrelocatable package"
      end
  | _ ->
    Misc.global_log "unexpected arg.";
    Printf.eprintf
      "Unexpected args: usage is '%s %s name version depends cmd...'\n%!" Globals.command cmd_name ;
    exit 2


let cmd =
  let args = ref [] in
  Arg.{
    cmd_name ;
    cmd_action = (fun () -> action !args) ;
    cmd_args = [
      [ "opamfile" ] , Arg.String (fun s ->
          CommandPostInstall.opamfile_arg := Some s),
      Ezcmd.info "filename of the opam package description";
      [], Anons (fun list -> args := list),
      Ezcmd.info "args"
    ];
    cmd_man = [];
    cmd_doc = "(opam hook) Backup the sources before building the package";
  }
