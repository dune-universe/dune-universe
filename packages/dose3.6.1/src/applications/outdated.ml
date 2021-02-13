(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s): Pietro Abate                                          *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
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
open Dose_algo
open Dose_doseparse

(*open DoseparseNoRpm*)

module Options = struct
  open OptParse

  let description =
    "Report packages that aren't installable in any futures of a repository"

  let options = OptParser.make ~description

  include StdOptions.MakeOptions (struct
    let options = options
  end)

  include StdOptions.DistcheckOptions

  ;;
  StdOptions.DistcheckOptions.add_options options

  include StdOptions.InputOptions

  ;;
  StdOptions.InputOptions.add_options ~default:["checkonly"] options

  include StdOptions.DistribOptions

  ;;
  StdOptions.DistribOptions.add_debian_options options

  let dump = StdOpt.store_true ()

  open OptParser

  ;;
  add options ~long_name:"dump" ~help:"Dump the cudf package list and exit" dump
end

include Util.Logging (struct
  let label = "dose_applications.outdated"
end)

let sync (sn, sv, v) p =
  let cn = CudfAdd.encode ("src/" ^ sn ^ "/" ^ sv) in
  { p with
    Cudf.provides = (cn, Some (`Eq, v)) :: p.Cudf.provides;
    Cudf.conflicts = (cn, Some (`Neq, v)) :: p.Cudf.conflicts
  }

let dummy (sn, _sv) pkg number equivs version =
  { Cudf.default_package with
    Cudf.package = pkg.Cudf.package;
    version;
    conflicts = [(pkg.Cudf.package, None)];
    provides = pkg.Cudf.provides;
    keep = pkg.Cudf.keep;
    pkg_extra =
      [ ("number", `String number);
        ("architecture", `String "dummy");
        ("equivs", `String (String.concat "," equivs));
        ("source", `String sn);
        ("sourcenumber", `String number) ]
  }

let evalsel getv target constr =
  let evalsel v = function
    | (`Eq, w) -> v = getv w
    | (`Geq, w) -> v >= getv w
    | (`Leq, w) -> v <= getv w
    | (`Gt, w) -> v > getv w
    | (`Lt, w) -> v < getv w
    | (`Neq, w) -> v <> getv w
  in
  match target with
  | `Hi v -> evalsel (getv v + 1) constr
  | `Lo v -> evalsel (getv v - 1) constr
  | `Eq v -> evalsel (getv v) constr
  | `In (_, v2) -> evalsel (getv v2 - 1) constr

let version_of_target getv = function
  | `Eq v -> getv v
  | `Hi v -> getv v + 1
  | `Lo v | `In (_, v) -> getv v - 1

(* the repository should contain only the most recent version of each
   package *)
let future ~options repository =
  let worktable = Hashtbl.create 1024 in
  let version_acc = ref [] in
  let constraints_table = Dose_debian.Evolution.constraints repository in
  let realpackages = Hashtbl.create 1023 in
  let clusters = Dose_debian.Debutil.cluster repository in
  (* for each cluster, I associate to it: its discriminants,
   * its cluster name and its binary version *)
  let cluster_iter (sn, _sv) l =
    List.iter
      (fun (version, _realversion, cluster) ->
        List.iter
          (fun pkg ->
            let pn = pkg#name in
            if Hashtbl.mem constraints_table pn then
              Hashtbl.add realpackages pn ())
          cluster ;
        let (versionlist, constr) =
          Dose_debian.Evolution.all_ver_constr constraints_table cluster
        in
        version_acc := versionlist @ !version_acc ;
        Hashtbl.add worktable (sn, version) (cluster, versionlist, constr))
      l
  in
  (* for each package name, that is not a real package,
   * I create a package with version 1 and I put it in a
   * cluster by itself *)
  let constraints_iter name constr =
    if not (Hashtbl.mem realpackages name) then (
      let vl = Dose_debian.Evolution.all_versions constr in
      let pkg =
        new Dose_debian.Packages.package
          ~name:("", Some name)
          ~version:("", Some "1")
          ~architecture:("", Some "all")
          []
      in
      let cluster = [pkg] in
      version_acc := vl @ !version_acc ;
      Hashtbl.add worktable (name, "1") (cluster, vl, constr))
  in
  (* if List.length checklist > 0 then
       List.iter (fun (sn,_,sv) ->
         begin try cluster_iter (sn,sv) (Hashtbl.find clusters (sn,sv)) with Not_found -> () end;
         begin try constraints_iter sn (Hashtbl.find constraints_table sn) with Not_found -> () end;
       ) checklist
     else *)
  Hashtbl.iter cluster_iter clusters ;
  Hashtbl.iter constraints_iter constraints_table ;
  Hashtbl.clear realpackages ;
  let versionlist = Util.list_unique ("1" :: !version_acc) in
  info "Total Names: %d" (Hashtbl.length worktable) ;
  info "Total versions: %d" (List.length versionlist) ;
  let tables =
    Dose_debian.Debcudf.init_tables ~options ~step:2 ~versionlist repository
  in
  let getv v = Dose_debian.Debcudf.get_cudf_version tables ("", v) in
  let pkgset =
    Hashtbl.fold
      (fun (sn, version) (cluster, vl, constr) acc0 ->
        let sync_index = ref 1 in
        let discr =
          Dose_debian.Evolution.discriminant (evalsel getv) vl constr
        in
        let acc0 =
          (* by assumption all packages in a cluster are syncronized *)
          List.fold_left
            (fun l pkg ->
              let p = Dose_debian.Debcudf.tocudf ~options tables pkg in
              CudfAdd.Cudf_set.add (sync (sn, version, 1) p) l)
            acc0
            cluster
        in
        (* the target version is always greater then all versions in equivs *)
        List.fold_left
          (fun acc1 (target, equiv) ->
            incr sync_index ;
            List.fold_left
              (fun acc2 pkg ->
                let p = Dose_debian.Debcudf.tocudf ~options tables pkg in
                let pv = p.Cudf.version in
                let target = Dose_debian.Evolution.align pkg#version target in
                let newv = version_of_target getv target in
                let number = Dose_debian.Evolution.string_of_range target in
                let equivs =
                  List.map Dose_debian.Evolution.string_of_range equiv
                in
                if newv > pv then
                  let d = dummy (sn, version) p number equivs newv in
                  if List.length cluster > 1 then
                    CudfAdd.Cudf_set.add
                      (sync (sn, version, !sync_index) d)
                      acc2
                  else CudfAdd.Cudf_set.add d acc2
                else acc2)
              acc1
              cluster)
          acc0
          discr)
      worktable
      CudfAdd.Cudf_set.empty
  in
  Hashtbl.clear worktable ;
  Hashtbl.clear constraints_table ;
  let universe = Cudf.load_universe (CudfAdd.Cudf_set.elements pkgset) in
  (universe, tables)

let outdated ?(dump = false) ?(failure = false) ?(explain = false)
    ?(summary = false) ~options repository =
  let (universe, tables) = future ~options repository in
  let universe_size = Cudf.universe_size universe in
  info "Total future: %d" universe_size ;
  if dump then (
    Cudf_printer.pp_preamble stdout Dose_debian.Debcudf.preamble ;
    print_newline () ;
    Cudf_printer.pp_universe stdout universe ;
    exit 0) ;
  let checklist =
    let to_cudf (p, v) =
      (p, Dose_debian.Debcudf.get_cudf_version tables (p, v))
    in
    if OptParse.Opt.is_set Options.checkonly then
      List.flatten
        (List.map
           (fun ((n, a), c) ->
             let (name, filter) =
               Dose_pef.Pefcudf.pefvpkg to_cudf ((n, a), c)
             in
             Cudf.lookup_packages ~filter universe name)
           (OptParse.Opt.get Options.checkonly))
    else []
  in
  let pp pkg =
    let (p, a) =
      let (n, a) = Dose_debian.Debcudf.get_real_name pkg.Cudf.package in
      if String.starts_with n "src/" then
        (Printf.sprintf "Source conflict (%s)" n, a)
      else (n, a)
    in
    let v =
      if pkg.Cudf.version > 0 then
        if String.starts_with pkg.Cudf.package "src/" then
          string_of_int pkg.Cudf.version
        else
          try Cudf.lookup_package_property pkg "number"
          with Not_found ->
            if pkg.Cudf.version mod 2 = 1 then
              let (_, _, v) =
                Dose_debian.Debcudf.get_real_version
                  tables
                  (pkg.Cudf.package, pkg.Cudf.version)
              in
              v
            else fatal "Real package without Debian Version"
      else "nan"
    in
    let l =
      List.filter_map
        (fun k ->
          try Some (k, (Cudf.lookup_package_property pkg k, true))
          with Not_found -> None)
        ["architecture"; "source"; "sourcenumber"; "equivs"]
    in
    (p, a, v, l)
  in
  let fmt = Format.std_formatter in
  Diagnostic.pp_out_version fmt ;
  if failure then Format.fprintf fmt "@[<v 1>report:@," ;
  let results = Diagnostic.default_result universe_size in
  let callback d =
    if summary then Diagnostic.collect results d ;
    if failure then Diagnostic.fprintf ~pp ~failure ~explain fmt d
  in
  let broken =
    if checklist <> [] then
      Depsolver.listcheck ~callback ~explain universe checklist
    else Depsolver.univcheck ~callback ~explain universe
  in
  if failure then Format.fprintf fmt "@]@." ;
  Format.fprintf fmt "total-packages: %d@." universe_size ;
  Format.fprintf fmt "broken-packages: %d@." broken ;
  if summary then
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results ;
  results

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  let options =
    match Option.get (Options.set_options `Deb) with
    | StdOptions.Deb o -> o
    | _ -> fatal "impossible"
  in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose) ;
  StdDebug.enable_bars
    (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck"; "Depsolver_int.init_solver"] ;
  StdDebug.enable_timers
    (OptParse.Opt.get Options.timers)
    ["Algo.Depsolver.solver"; "Algo.Depsolver.init"] ;
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet) ;
  (*let checklist = OptParse.Opt.opt Options.checkonly in*)
  let failure = OptParse.Opt.get Options.failure in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in
  let dump = OptParse.Opt.get Options.dump in
  let archs =
    if not (Option.is_none options.Dose_debian.Debcudf.native) then
      Option.get options.Dose_debian.Debcudf.native
      :: options.Dose_debian.Debcudf.foreign
    else []
  in
  let packagelist = Dose_debian.Packages.input_raw ~archs args in
  let result = outdated ~summary ~failure ~explain ~dump ~options packagelist in
  if result.Diagnostic.missing = 0 && result.Diagnostic.conflict = 0 then 0
    (* no broken packages *)
  else 1

(* at least one broken package *)

;;
StdUtils.if_application
  ~alternatives:
    ["dose-outdated"; "dose3-outdated"; "edos-outdated"; "deb-outdated"]
  "outdated"
  main
