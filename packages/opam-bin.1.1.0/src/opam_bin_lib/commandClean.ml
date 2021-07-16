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

let cmd_name = "clean"

let clean_log () =
  if Sys.file_exists Globals.opambin_log then
    Sys.remove Globals.opambin_log ;
  if Sys.file_exists Globals.opambin_info then
    Sys.remove Globals.opambin_info ;
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

let clean_unused ~cache ~archive () =
  let root = Globals.opam_dir in
  if cache then begin
    (* cache: whether we should remove all files from
       $OPAMROOT/plugins/opam-bin/share that are not used anywhere in the
       switches. *)
    let inodes = Hashtbl.create 11111 in
    EzFile.iter_dir ~f:(fun switch ->
        let switch_dir = root // switch in
        if Sys.is_directory switch_dir && switch <> "plugins" then
          EzFile.make_select EzFile.iter_dir ~deep:true ~f:(fun file ->
              let file = switch_dir // file in
              let st = Unix.lstat file in
              let inode = st.Unix.st_dev, st.Unix.st_ino in
              Hashtbl.add inodes inode file;
            ) switch_dir
      ) root;

    EzFile.make_select ~deep:true ~kinds:[Unix.S_REG]
      EzFile.iter_dir ~f:(fun file ->
          let file = Globals.opambin_share_dir // file in
          let st = Unix.lstat file in
          let inode = st.Unix.st_dev, st.Unix.st_ino in
          try
            let use_for_file = Hashtbl.find inodes inode in
            Printf.eprintf "%s used by %s\n%!" file use_for_file
          with Not_found ->
            Printf.eprintf "removing %s\n%!" file
        ) Globals.opambin_share_dir;
  end;

  if archive then begin
    (* archive: whether we should remove all binary archives that are not used
       anywhere in any of the switches.
       HINT: iterate on $OPAMROOT/$SWITCH/etc/opam-bin/packages/$PACKAGE to get
       all the versions that are currently being used. Then, remove all the
       other ones from $OPAMROOT/opam-bin/store/repo/packages/,
       $OPAMROOT/opam-bin/store/archives/ and
       $OPAMROOT/opam-bin/cache/
    *)
    let used_archives = Hashtbl.create 1000 in
    let used_nv = Hashtbl.create 1000 in
    EzFile.iter_dir ~f:(fun switch ->
        let dir = root // switch // "etc" // "opam-bin" // "packages" in
        let packages = try EzFile.readdir dir with _ -> [||] in
        Array.iter (fun package ->
            let version = EzFile.read_file (dir // package) in
            let nv = Printf.sprintf "%s.%s" package version in
            Hashtbl.add used_nv nv switch;
            Hashtbl.add used_archives ( nv ^ "-bin.tar.gz" ) switch;
          ) packages
      ) root;

    let share_cache = try ignore ( Sys.getenv "OPAMBIN_SHARE_CACHE" ); true
      with Not_found -> false in

    let cached_file ~file =
      let bin_md5 = Digest.to_hex ( Digest.file file ) in
      Globals.opambin_cache_dir //
      "md5" // String.sub bin_md5 0 2 // bin_md5
    in
    EzFile.iter_dir ~f:(fun archive ->
        let file =  Globals.opambin_store_archives_dir // archive in
        try
          let switch = Hashtbl.find used_archives archive in
          Printf.eprintf "archive %s used in switch %S\n" archive switch;
          if share_cache && !!Config.share_enabled then
            let cached_file = cached_file ~file in
            if Sys.file_exists cached_file then begin
              Printf.eprintf "hardlinking %s to %s\n%!" file cached_file;
              Sys.remove cached_file;
              Unix.link file cached_file
            end
        with Not_found ->
          Printf.eprintf "removing %s\n%!" file;
          let cached_file = cached_file ~file in
          Sys.remove file;
          if Sys.file_exists cached_file then begin
            Printf.eprintf "removing %s\n%!" cached_file;
            Sys.remove cached_file;
          end
      ) Globals.opambin_store_archives_dir;

    EzFile.iter_dir ~f:(fun package ->
        let package_dir =
          Globals.opambin_store_repo_packages_dir // package in
        EzFile.iter_dir ~f:(fun nv ->
            try
              let switch = Hashtbl.find used_nv nv in
              Printf.eprintf "package %s used in switch %s\n%!" nv switch
            with Not_found ->
              Printf.eprintf "removing package %s\n%!" nv;
              Misc.call [| "rm"; "-rf";
                           package_dir // nv |]
          ) package_dir
      ) Globals.opambin_store_repo_packages_dir;

  end;
  ()

let action args =
  match args with
  | [] -> clean_all ()
  | _ ->
    List.iter (function
        | "all" -> clean_all ()
        | "log" -> clean_log ()
        | "store" -> clean_store ()
        | "unused" -> clean_unused ~cache:true ~archive:true ()
        | "unused-cache" -> clean_unused ~cache:true ~archive:false ()
        | "unused-archive" -> clean_unused ~cache:false ~archive:true ()
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
  cmd_man = [
    `S "DESCRIPTION" ;
    `Blocks [
      `P "Use 'all', 'log', 'store', 'unused', 'unused-cached' or 'unused-archive' as arguments.";
    ]
  ];
  cmd_doc = "clear all packages and archives from the cache and store";
}
