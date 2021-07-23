(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Dose_common
open Dose_extra

include Util.Logging (struct
  let label = "doseparse.stdLoaders"
end)

let load_list_timer = Util.Timer.create "Load"

let deb_load_list_timer = Util.Timer.create "Load.Debian"

let deb_load_source_timer = Util.Timer.create "Load.DebianSource"

(* a list of the raw package types for all input except Cudf itself *)
(* currently, only Deb and DebSrc are used *)
type rawpackage =
  | Deb of Dose_debian.Packages.package
  | DebSrc of Dose_debian.Sources.source
  | Pef of Dose_pef.Packages.package
  | Opam of Dose_opam2.Packages.package
  | Npm of Dose_npm.Packages.package
  | Edsp of Dose_debian.Packages.package
  | Csw of Dose_opencsw.Packages.package

(** read a debian Packages file - compressed or not *)
let read_deb ?filter ?(extras = []) fname =
  Dose_debian.Packages.input_raw ?filter ~extras [fname]

(* fll = file list list
 * dll = deb packages list list
 * cll = cudf package list list
 *)
let deb_load_list options ?(status = []) ?(raw = false) dll =
  Util.Timer.start deb_load_list_timer ;
  let noindep = options.Dose_debian.Debcudf.drop_bd_indep in
  let noarch = options.Dose_debian.Debcudf.drop_bd_arch in
  let profiles = options.Dose_debian.Debcudf.profiles in
  let pkgll =
    List.map
      (List.map (function
          | Deb p -> p
          | DebSrc p ->
              if Option.is_none options.Dose_debian.Debcudf.native then
                fatal
                  "--deb-native-arch was not specified while treating Debian \
                   Sources File" ;
              let buildarch = Option.get options.Dose_debian.Debcudf.native in
              let hostarch = Option.get options.Dose_debian.Debcudf.host in
              Dose_debian.Sources.src2pkg
                ~noindep
                ~noarch
                ~profiles
                buildarch
                hostarch
                p
          | _ -> fatal "cannot handle input"))
      dll
  in
  let pkgl = List.flatten pkgll in
  let pkgl =
    if status = [] then pkgl else Dose_debian.Packages.merge status pkgl
  in
  let tables = Dose_debian.Debcudf.init_tables ~options pkgl in
  let from_cudf (p, i) = Dose_debian.Debcudf.get_real_version tables (p, i) in
  let to_cudf (p, v) =
    (p, Dose_debian.Debcudf.get_cudf_version tables (p, v))
  in
  let cll =
    List.map
      (fun l ->
        List.map
          (Dose_debian.Debcudf.tocudf tables ~options)
          (Dose_debian.Packages.merge status l))
      pkgll
  in
  (* if requested, connect all cudf packages representing binary packages to
   * the cudf packages representing the source package they each build from,
   * respectively *)
  let cll =
    if options.Dose_debian.Debcudf.builds_from then
      let univ =
        Cudf.load_universe
          (CudfAdd.Cudf_set.elements
             (List.fold_right
                (List.fold_right CudfAdd.Cudf_set.add)
                cll
                CudfAdd.Cudf_set.empty))
      in
      List.map2
        (List.map2 (fun cudfpkg debpkg ->
             match debpkg with
             | Deb _ ->
                 let srcpkg =
                   try Dose_debian.Sources.get_src_package univ cudfpkg
                   with Dose_debian.Sources.NotfoundSrc ->
                     failwith
                       (Printf.sprintf
                          "cannot find source for binary package %s"
                          (CudfAdd.string_of_package cudfpkg))
                 in
                 (* connect to source package as "builds-from" *)
                 let srcdep =
                   (srcpkg.Cudf.package, Some (`Eq, srcpkg.Cudf.version))
                 in
                 { cudfpkg with
                   Cudf.depends = [srcdep] :: cudfpkg.Cudf.depends
                 }
             | DebSrc _ -> cudfpkg
             | _ -> failwith "impossible"))
        cll
        dll
    else cll
  in
  let preamble = Dose_debian.Debcudf.preamble in
  let request = Cudf.default_request in
  let rawll = if raw && status = [] then Some dll else None in
  let global_constraints = Dose_debian.Debcudf.get_essential ~options tables in
  let l =
    (preamble, cll, request, from_cudf, to_cudf, rawll, global_constraints)
  in
  Util.Timer.stop deb_load_list_timer l

let npm_load_list file =
  let (_request, pkglist) = Dose_npm.Packages.input_raw file in
  let tables =
    Dose_pef.Pefcudf.init_tables Dose_versioning.SemverNode.compare pkglist
  in
  let from_cudf (p, i) = Dose_pef.Pefcudf.get_real_version tables (p, i) in
  let to_cudf (p, v) = (p, Dose_pef.Pefcudf.get_cudf_version tables (p, v)) in
  let cl = List.map (Dose_pef.Pefcudf.tocudf tables) pkglist in
  let preamble = Dose_npm.Npmcudf.preamble in
  let request = Cudf.default_request in
  (*
  let request = Dose_npm.Npmcudf.requesttocudf tables (Cudf.load_universe cl) request in
  *)
  (preamble, [cl; []], request, from_cudf, to_cudf, None, [])

let opam_load_list ?options file =
  let (request, pkglist) = Dose_opam2.Packages.input_raw file in
  let tables =
    Dose_pef.Pefcudf.init_tables Dose_versioning.Debian.compare pkglist
  in
  let from_cudf (p, i) = Dose_pef.Pefcudf.get_real_version tables (p, i) in
  let to_cudf (p, v) = (p, Dose_pef.Pefcudf.get_cudf_version tables (p, v)) in
  let options =
    match options with
    | None ->
        { Dose_opam2.Opamcudf.default_options with
          Dose_opam2.Opamcudf.switch = request.Dose_opam2.Packages.switch;
          switches = request.Dose_opam2.Packages.switches;
          profiles = request.Dose_opam2.Packages.profiles
        }
    | Some opt -> opt
  in
  let cl =
    List.flatten (List.map (Dose_opam2.Opamcudf.tocudf ~options tables) pkglist)
  in
  let preamble = Dose_opam2.Opamcudf.preamble in
  let request =
    Dose_opam2.Opamcudf.requesttocudf tables (Cudf.load_universe cl) request
  in
  (preamble, [cl; []], request, from_cudf, to_cudf, None, [])

let pef_load_list ?compare dll =
  let compare =
    match compare with Some c -> c | None -> Dose_versioning.Debian.compare
  in
  let extras = [("maintainer", ("maintainer", `String None))] in
  let pkglist = List.flatten dll in
  let tables = Dose_pef.Pefcudf.init_tables compare pkglist in
  let from_cudf (p, i) = Dose_pef.Pefcudf.get_real_version tables (p, i) in
  let to_cudf (p, v) = (p, Dose_pef.Pefcudf.get_cudf_version tables (p, v)) in
  let cll =
    List.map (fun l -> List.map (Dose_pef.Pefcudf.tocudf ~extras tables) l) dll
  in
  let preamble = Dose_pef.Pefcudf.preamble in
  let request = Cudf.default_request in
  (preamble, cll, request, from_cudf, to_cudf, None, [])

let csw_load_list dll =
  let pkglist = List.flatten dll in
  let tables = Dose_opencsw.Cswcudf.init_tables pkglist in
  let from_cudf (p, i) =
    (p, None, Dose_opencsw.Cswcudf.get_real_version tables (p, i))
  in
  let to_cudf (p, v) =
    (p, Dose_opencsw.Cswcudf.get_cudf_version tables (p, v))
  in
  let cll =
    List.map (fun l -> List.map (Dose_opencsw.Cswcudf.tocudf tables) l) dll
  in
  let preamble = Dose_opencsw.Cswcudf.preamble in
  let request = Cudf.default_request in
  (preamble, cll, request, from_cudf, to_cudf, None, [])

let edsp_load_list options file =
  let (request, pkglist) = Dose_debian.Edsp.input_raw file in
  let (native_arch, foreign_archs) =
    StdUtils.get_architectures
      request.Dose_debian.Edsp.architecture
      request.Dose_debian.Edsp.architectures
      options.Dose_debian.Debcudf.native
      (match options.Dose_debian.Debcudf.foreign with
      | [] -> None
      | l -> Some l)
  in
  let options =
    { options with
      Dose_debian.Debcudf.native = native_arch;
      Dose_debian.Debcudf.foreign = foreign_archs
    }
  in
  let tables = Dose_debian.Debcudf.init_tables ~options pkglist in
  let preamble =
    let l = List.map snd Dose_debian.Edsp.extras_tocudf in
    Dose_common.CudfAdd.add_properties Dose_debian.Debcudf.preamble l
  in
  let univ = Hashtbl.create ((2 * List.length pkglist) - 1) in
  let cudfpkglist =
    List.filter_map
      (fun pkg ->
        let p = Dose_debian.Edsp.tocudf tables ~options pkg in
        if not (Hashtbl.mem univ (p.Cudf.package, p.Cudf.version)) then (
          Hashtbl.add univ (p.Cudf.package, p.Cudf.version) pkg ;
          Some p)
        else (
          warning
            "Duplicated package (same version, name and architecture) : \
             (%s,%s,%s)"
            pkg#name
            pkg#version
            pkg#architecture ;
          None))
      pkglist
  in
  let request =
    Dose_debian.Edsp.requesttocudf
      tables
      (Cudf.load_universe cudfpkglist)
      request
  in
  let to_cudf (p, v) =
    (p, Dose_debian.Debcudf.get_cudf_version tables (p, v))
  in
  let from_cudf (p, i) = Dose_debian.Debcudf.get_real_version tables (p, i) in
  let global_constraints = Dose_debian.Debcudf.get_essential ~options tables in
  ( preamble,
    [cudfpkglist; []],
    request,
    from_cudf,
    to_cudf,
    None,
    global_constraints )

let edsp_load_universe options file =
  let (pr, l, r, f, t, w, e) = edsp_load_list options file in
  (pr, Cudf.load_universe (List.hd l), r, f, t, w, e)

(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe options ?(raw = false) l =
  let (pr, cll, r, f, t, w, e) = deb_load_list options ~raw l in
  (pr, Cudf.load_universe (List.flatten cll), r, f, t, w, e)

(** parse a cudf file and return a triple (preamble,package list,request
    option). If the package is not valid returns an empty list of packages *)
let parse_cudf doc =
  try
    let p = Cudf_parser.from_IO_in_channel (Input.open_file doc) in
    Cudf_parser.parse p
  with
  | Input.File_empty -> (None, [], None)
  | Cudf_parser.Parse_error (msg, loc) ->
      fatal
        "Error while parsing CUDF from %s (%s): %s"
        doc
        (Format822.string_of_loc loc)
        msg
  | Cudf.Constraint_violation _ as exn ->
      fatal "Error while loading CUDF from %s: %s" doc (Printexc.to_string exn)

(** parse a cudf file and return a triple (preamble,universe,request option).
    If the package is not valid return an empty list of packages *)
let load_cudf doc =
  let ch = Input.open_file doc in
  let l =
    try
      let p = Cudf_parser.from_IO_in_channel ch in
      Cudf_parser.load p
    with
    | Input.File_empty -> (None, Cudf.load_universe [], None)
    | Cudf_parser.Parse_error (msg, loc) ->
        fatal
          "Error while parsing CUDF from %s (%s): %s"
          doc
          (Format822.string_of_loc loc)
          msg
    | Cudf.Constraint_violation _ as exn ->
        fatal
          "Error while loading CUDF file %s:\n%s"
          doc
          (Printexc.to_string exn)
  in
  Input.close_ch ch ;
  l

let cudf_load_list file =
  let (preamble, pkglist, request) =
    match parse_cudf file with
    | (None, pkglist, None) ->
        (Cudf.default_preamble, pkglist, Cudf.default_request)
    | (None, pkglist, Some req) -> (Cudf.default_preamble, pkglist, req)
    | (Some p, pkglist, None) -> (p, pkglist, Cudf.default_request)
    | (Some p, pkglist, Some req) -> (p, pkglist, req)
  in
  let from_cudf (p, i) = (p, None, string_of_int i) in
  let to_cudf (p, v) = (p, int_of_string v) in
  (preamble, [pkglist; []], request, from_cudf, to_cudf, None, [])

let cudf_load_universe file =
  let (pr, l, r, f, t, w, _) = cudf_load_list file in
  (pr, Cudf.load_universe (List.hd l), r, f, t, w, [])

let unpack_l expected l =
  List.fold_left
    (fun acc (t, (_, _, _, _, f), _) ->
      if t = expected then f :: acc
      else fatal "cannot handle input %s" (Url.scheme_to_string t))
    []
    l

let unpack expected = function
  | (t, (_, _, _, _, f), _) when t = expected -> f
  | _ -> "cannot handle input"

let deb_parse_input options ?(status = []) ?(raw = false) urilist =
  let archs =
    if not (Option.is_none options.Dose_debian.Debcudf.native) then
      Option.get options.Dose_debian.Debcudf.native
      :: options.Dose_debian.Debcudf.foreign
    else []
  in
  let dll =
    List.map
      (fun l ->
        List.fold_left
          (fun acc (t, (_, _, _, _, f), _) ->
            match t with
            | `Deb ->
                List.fold_left
                  (fun acc p -> Deb p :: acc)
                  acc
                  (Dose_debian.Packages.input_raw ~archs [f])
            | `DebSrc ->
                List.fold_left
                  (fun acc p -> DebSrc p :: acc)
                  acc
                  (Dose_debian.Sources.input_raw ~archs [f])
            | _ -> fatal "cannot handle input")
          []
          l)
      urilist
  in
  deb_load_list options ~status ~raw dll

let pef_parse_input ?compare urilist =
  let extras = [("maintainer", None)] in
  let dll =
    List.map
      (fun l ->
        let filelist = unpack_l `Pef l in
        Dose_pef.Packages.input_raw ~extras filelist)
      urilist
  in
  pef_load_list ?compare dll

let npm_parse_input urilist =
  match urilist with
  | [[p]] when unpack `Npm p = "-" -> fatal "no stdin for npm yet"
  | [[p]] -> npm_load_list (unpack `Npm p)
  | l ->
      if List.length (List.flatten l) > 1 then
        warning "more than one npm request file specified on the command line" ;
      let p = List.hd (List.flatten l) in
      npm_load_list (unpack `Npm p)

let opam_parse_input ?options urilist =
  match urilist with
  | [[p]] when unpack `Opam p = "-" -> fatal "no stdin for opam yet"
  | [[p]] -> opam_load_list ?options (unpack `Opam p)
  | l ->
      if List.length (List.flatten l) > 1 then
        warning "more than one opam request file specified on the command line" ;
      let p = List.hd (List.flatten l) in
      opam_load_list ?options (unpack `Opam p)

let csw_parse_input urilist =
  let dll =
    List.map
      (fun l ->
        let filelist = unpack_l `Csw l in
        Dose_opencsw.Packages.input_raw filelist)
      urilist
  in
  csw_load_list dll

let cudf_parse_input urilist =
  match urilist with
  | [[p]] when unpack `Cudf p = "-" -> fatal "no stdin for cudf yet"
  | [[p]] -> cudf_load_list (unpack `Cudf p)
  | l ->
      if List.length (List.flatten l) > 1 then
        warning "more than one cudf specified on the command line" ;
      let p = List.hd (List.flatten l) in
      cudf_load_list (unpack `Cudf p)

let edsp_parse_input options urilist =
  match urilist with
  | [[p]] when unpack `Edsp p = "-" -> fatal "no stdin for edsp yet"
  | [[p]] -> edsp_load_list options (unpack `Edsp p)
  | l ->
      if List.length (List.flatten l) > 1 then
        warning "more than one cudf specified on the command line" ;
      let p = List.hd (List.flatten l) in
      edsp_load_list options (unpack `Edsp p)

(* Check that all uris are of type that is an instance of scheme *)
(* If yes return that instance of scheme, and the list of paths  *)
(* in uris.                                                      *)

(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?(options = None) ?(raw = false) ?compare urilist =
  let filelist = List.map (List.map Input.parse_uri) urilist in
  match (Input.guess_format urilist, options) with
  | (`Cudf, None) -> cudf_parse_input filelist
  | (`Deb, None) | (`DebSrc, None) ->
      deb_parse_input Dose_debian.Debcudf.default_options ~raw filelist
  | (`Pef, None) -> pef_parse_input ?compare filelist
  | (`Deb, Some (StdOptions.Deb opt)) | (`DebSrc, Some (StdOptions.Deb opt)) ->
      deb_parse_input opt ~raw filelist
  (*  |`Edsp, Some (StdOptions.Edsp opt) -> edsp_parse_input opt filelist *)
  | (`Edsp, _) -> edsp_parse_input Dose_debian.Debcudf.default_options filelist
  | (`Opam, _) -> opam_parse_input filelist
  | (`Npm, _) -> npm_parse_input filelist
  (* |`Opam, Some (StdOptions.Opam options) -> opam_parse_input ~options filelist *)
  | (`Pef, Some (StdOptions.Pef _)) -> pef_parse_input ?compare filelist
  | (`Csw, None) -> csw_parse_input filelist
  | (`Hdlist, None) -> fatal "hdlist Not supported."
  | (`Synthesis, None) -> fatal "synthesis input format not supported."
  | (s, _) -> fatal "%s Not supported" (Url.scheme_to_string s)

let supported_formats () =
  ["cudf://"; "deb://"; "deb://-"; "eclipse://"; "pef://"]

(** return a list of Debian packages from a debian source file *)
let deb_load_source ?filter ?(dropalternatives = false) ?(profiles = [])
    ?(noindep = false) ?(noarch = false) buildarch hostarch sourcefile =
  Util.Timer.start deb_load_source_timer ;
  let l =
    Dose_debian.Sources.input_raw ?filter ~archs:[hostarch] [sourcefile]
  in
  let r =
    Dose_debian.Sources.sources2packages
      ~dropalternatives
      ~noindep
      ~noarch
      ~profiles
      buildarch
      hostarch
      l
  in
  Util.Timer.stop deb_load_source_timer r

(** parse and merge a list of files into a cudf package list *)
let load_list ?(options = None) ?(raw = false) ?compare urilist =
  info "Parsing and normalizing..." ;
  Util.Timer.start load_list_timer ;
  let u = parse_input ~options ~raw ?compare urilist in
  Util.Timer.stop load_list_timer u

(** parse and merge a list of files into a cudf universe *)
let load_universe ?(options = None) ?(raw = false) ?compare uris =
  info "Parsing and normalizing..." ;
  Util.Timer.start load_list_timer ;
  let (pr, cll, r, f, t, w, e) = parse_input ~options ~raw ?compare [uris] in
  let u = (pr, Cudf.load_universe (List.flatten cll), r, f, t, w, e) in
  Util.Timer.stop load_list_timer u
