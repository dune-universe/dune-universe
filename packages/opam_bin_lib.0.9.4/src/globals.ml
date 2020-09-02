(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzFile.OP

let command = "opam-bin"
let about =
  Printf.sprintf "%s %s by OCamlPro SAS <contact@ocamlpro.com>"
    command Version.version

let home_dir = try Sys.getenv "HOME" with Not_found ->
  Printf.eprintf "Error: HOME variable not defined\n%!";
  exit 2


let opam_dir = try
    Sys.getenv "OPAMROOT"
  with Not_found -> home_dir // ".opam"

let command_exe = command ^ ".exe"
let command_log = command ^ ".log"

let opam_cache_dir = opam_dir // "download-cache"
let opam_repo_dir = opam_dir // "repo"
let opam_plugins_dir = opam_dir // "plugins"
let opam_plugins_bin_dir = opam_plugins_dir // "bin"
let opambin_dir = opam_plugins_dir // command
let opambin_bin = opambin_dir // command_exe
let opambin_log = opambin_dir // command_log
let opambin_store_dir = opambin_dir // "store"
let opambin_share_dir = opambin_dir // "share"
let opambin_cache_dir = opambin_dir // "cache"
let opambin_header_html = opambin_dir // "header.html"
let opambin_trailer_html = opambin_dir // "trailer.html"
let opambin_store_archives_dir = opambin_store_dir // "archives"
let opambin_store_repo_dir = opambin_store_dir // "repo"
let opambin_store_repo_packages_dir = opambin_store_repo_dir // "packages"
let opambin_patches_dir = opambin_dir // "patches"
let opambin_patches_temp_dir = opambin_dir // "patches.tmp"

let opam_config_file = opam_dir // "config"
let opam_config_file_backup = opam_config_file ^ ".1"
let opam_opambin_repo = "local-bin"

let opam_switch_prefix =
  lazy (
    try
      Sys.getenv "OPAM_SWITCH_PREFIX"
    with Not_found ->
      Printf.eprintf
        "Error in %s: OPAM_SWITCH_PREFIX not defined.\n%!"
        command ;
      exit 2
  )
let opam_switch_prefix () = Lazy.force opam_switch_prefix

let opam_switch_dir = opam_switch_prefix
let opam_switch_internal_dir () =
  opam_switch_dir () // ".opam-switch"

let opam_switch_internal_config_dir () =
  opam_switch_internal_dir () // "config"

let opambin_switch_temp_dir () =
  opam_switch_internal_dir () // command

(* Where bin versions are stored to solve deps *)
let opambin_switch_packages_dir () =
  opam_switch_dir () // "etc" // command // "packages"


(* names of the files created in the package `files` sub-dir *)
let package_version = "bin-package.version"
let package_config = "bin-package.config"
let package_info = "bin-package.info"

let marker_skip = ".binskip"
let marker_cached = "_bincached"
let marker_source = ".binsource"
let marker_opam = ".binopam"
let marker_patch = ".binpatch"

let backup_marker ~name ext =
  opambin_switch_temp_dir () // ( name ^ ext )
let backup_skip ~name = backup_marker ~name ".skip"
let backup_opam ~name = backup_marker ~name ".opam"
let backup_source ~name = backup_marker ~name ".source"
let backup_patch ~name = backup_marker ~name ".patch"


let config_file = opambin_dir // "config"

let curdir = Sys.getcwd ()

let system = "debian-buster-x86_64"


(*

File structure:

$HOME/.opam
   plugins/opam-bin/
     opam-bin.exe
     opam-bin.log
     cache/
     store/
       archives/
       repo/
         packages/

$OPAM_SWITCH_PREFIX/
   etc/opam-bin/packages/$NAME

*)
