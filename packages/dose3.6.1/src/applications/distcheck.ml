(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2012 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

open! ExtLib
open Dose_common
open Dose_algo
open Dose_doseparse

module Options = struct
  open OptParse

  let description = "Compute the list broken packages in a repository"

  let options = OptParser.make ~description

  include StdOptions.MakeOptions (struct
    let options = options
  end)

  let lowmem = StdOpt.store_false ()

  let coinst = StdDebian.vpkglist_option ()

  let fields = StdOptions.str_list_option ()

  include StdOptions.DistcheckOptions

  ;;
  StdOptions.DistcheckOptions.add_options options

  ;;
  StdOptions.DistcheckOptions.add_option
    options
    ~long_name:"coinst"
    ~help:"Check if these packages are coinstallable"
    coinst

  ;;
  StdOptions.DistcheckOptions.add_option
    options
    ~long_name:"fields"
    ~help:"Print additional fields if available"
    fields

  ;;
  StdOptions.DistcheckOptions.add_option
    options
    ~long_name:"lowmem"
    ~help:"Serialise multiple distcheck runs to save memory"
    lowmem

  include StdOptions.InputOptions

  ;;
  let default = "dot" :: StdOptions.InputOptions.default_options in
  StdOptions.InputOptions.add_options ~default options

  include StdOptions.OutputOptions

  ;;
  StdOptions.OutputOptions.add_options options

  include StdOptions.DistribOptions

  ;;
  let default =
    List.fold_left
      List.remove
      StdOptions.DistribOptions.default_options
      [ "deb-host-arch";
        "deb-drop-b-d-indep";
        "deb-drop-b-d-arch";
        "deb-profiles" ]
  in
  StdOptions.DistribOptions.add_debian_options ~default options ;
  StdOptions.DistribOptions.add_opam_options ~default options
end

include Util.Logging (struct
  let label = "dose_applications.distcheck"
end)

let timer = Util.Timer.create "Solver"

(* implicit prefix of resources derived from name of executable *)
(* (input_format * add_format ?) *)
let guess_format t l =
  match Filename.basename Sys.argv.(0) with
  | "debcheck" | "dose-debcheck" -> (`Deb, true)
  | "eclipsecheck" | "dose-eclipsecheck" -> (`Pef, true)
  | "rpmcheck" | "dose-rpmcheck" -> (`Synthesis, true)
  | _ when OptParse.Opt.is_set t ->
      (Url.scheme_of_string (OptParse.Opt.get t), true)
  | _ -> (Input.guess_format [l], false)

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let inputlist = posargs @ OptParse.Opt.get Options.foreground in
  let (input_type, implicit) = guess_format Options.inputtype inputlist in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose) ;
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) ["Solver"; "Load"] ;
  StdDebug.enable_bars
    (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck"; "Depsolver_int.init_solver"] ;
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet) ;
  let options = Options.set_options input_type in
  let (fg, bg) = Options.parse_cmdline (input_type, implicit) posargs in
  let (_, pkgll, _, from_cudf, to_cudf, _, global_constraints) =
    StdLoaders.load_list ~options [fg; bg]
  in
  let (fg_pkglist, bg_pkglist) =
    match pkgll with [fg; bg] -> (fg, bg) | _ -> assert false
  in
  let fg_pkglist =
    if OptParse.Opt.is_set Options.latest then
      CudfAdd.latest ~n:(OptParse.Opt.get Options.latest) fg_pkglist
    else fg_pkglist
  in
  let fn = List.length fg_pkglist in
  let bn = List.length bg_pkglist in
  let universe =
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    Cudf.load_universe (CudfAdd.Cudf_set.elements s)
  in
  let universe_size = Cudf.universe_size universe in
  info "Cudf Universe: %d packages" universe_size ;
  (* we get back a bit of memory *)
  if universe_size > 500000 then Gc.full_major () ;
  if OptParse.Opt.is_set Options.checkonly && OptParse.Opt.is_set Options.coinst
  then fatal "--checkonly and --coinst cannot be specified together" ;
  let checklist =
    if OptParse.Opt.is_set Options.checkonly then (
      info "--checkonly specified, consider all packages as background packages" ;
      let co = OptParse.Opt.get Options.checkonly in
      match
        List.flatten
          (List.filter_map
             (fun ((n, a), c) ->
               try
                 let (name, filter) =
                   Dose_pef.Pefcudf.pefvpkg to_cudf ((n, a), c)
                 in
                 Some (Cudf.lookup_packages ~filter universe name)
               with Not_found -> None)
             co)
      with
      | [] ->
          fatal
            "Cannot find any package corresponding to the selector %s"
            (Util.string_of_list ~sep:", " Dose_pef.Printer.string_of_vpkg co)
      | l -> l)
    else fg_pkglist
  in
  let coinstlist =
    if OptParse.Opt.is_set Options.coinst then (
      info "--coinst specified, consider all packages as background packages" ;
      let co = OptParse.Opt.get Options.coinst in
      List.map
        (fun ((n, a), c) ->
          match
            let (name, filter) = Dose_pef.Pefcudf.pefvpkg to_cudf ((n, a), c) in
            Cudf.lookup_packages ~filter universe name
          with
          | [] ->
              fatal
                "Cannot find any package corresponding to the selector %s"
                (Dose_pef.Printer.string_of_vpkg ((n, a), c))
          | l -> l)
        co)
    else []
  in
  let fields =
    if OptParse.Opt.is_set Options.fields then OptParse.Opt.get Options.fields
    else []
  in
  let pp = CudfAdd.pp ~fields from_cudf in
  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failure in
  let success = OptParse.Opt.get Options.success in
  let explain_summary = OptParse.Opt.get Options.explain in
  let minimal = OptParse.Opt.get Options.minimal in
  let condense = OptParse.Opt.get Options.condense in
  let summary = OptParse.Opt.get Options.summary in
  let explain =
    if summary then true
    else if success || failure then OptParse.Opt.get Options.explain
    else false
  in
  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      let f =
        let s = OptParse.Opt.get Options.outfile in
        if OptParse.Opt.is_set Options.outdir then
          let d = OptParse.Opt.get Options.outdir in
          Filename.concat d s
        else s
      in
      let oc = open_out f in
      Format.formatter_of_out_channel oc
    else Format.std_formatter
  in
  let results = Diagnostic.default_result universe_size in
  Diagnostic.pp_out_version fmt ;
  (match options with
  | Some (StdOptions.Deb o) | Some (StdOptions.Edsp o) ->
      if Option.is_some o.Dose_debian.Debcudf.native then
        Format.fprintf
          fmt
          "native-architecture: %s@."
          (Option.get o.Dose_debian.Debcudf.native) ;
      if List.length o.Dose_debian.Debcudf.foreign > 0 then
        Format.fprintf
          fmt
          "foreign-architecture: %s@."
          (String.concat "," o.Dose_debian.Debcudf.foreign)
  | _ -> ()) ;
  if failure || success then Format.fprintf fmt "@[<v 1>report:@," ;
  let callback d =
    let pp =
      if input_type = `Cudf then fun pkg -> pp ~decode:(fun x -> x) pkg
      else fun pkg -> pp ?decode:None pkg
    in
    if summary then Diagnostic.collect results d ;
    (if (not (Diagnostic.is_solution d)) && OptParse.Opt.get Options.dot then
     let dir = OptParse.Opt.opt Options.outdir in
     Diagnostic.print_dot ~pp ~addmissing:explain ?dir d) ;
    if failure || success then
      Diagnostic.fprintf ~pp ~failure ~success ~explain ~minimal ~condense fmt d
  in
  let pp = pp ?decode:None in
  Util.Timer.start timer ;
  if OptParse.Opt.is_set Options.coinst && List.length coinstlist > 0 then (
    let rl =
      Depsolver.edos_coinstall_prod ~global_constraints universe coinstlist
    in
    let nbt =
      List.length (List.filter (fun r -> not (Diagnostic.is_solution r)) rl)
    in
    let number_checks = List.length rl in
    ignore (Util.Timer.stop timer ()) ;
    List.iter callback rl ;
    if failure || success then Format.fprintf fmt "@]@." ;
    Format.fprintf fmt "total-packages: %d@." universe_size ;
    Format.fprintf fmt "total-tuples: %d@." number_checks ;
    Format.fprintf fmt "broken-tuples: %d@." nbt ;
    nbt)
  else
    let univcheck =
      if OptParse.Opt.get Options.lowmem then Depsolver.univcheck
      else Depsolver.univcheck_lowmem
    in
    let nbp =
      if OptParse.Opt.is_set Options.checkonly && List.length checklist = 0 then
        0
      else if OptParse.Opt.is_set Options.checkonly || not (bg_pkglist = [])
      then
        Depsolver.listcheck
          ~global_constraints
          ~callback
          ~explain
          universe
          checklist
      else univcheck ~global_constraints ~callback ~explain universe
    in
    ignore (Util.Timer.stop timer ()) ;
    if failure || success then Format.fprintf fmt "@]@." ;
    let (nb, nf) =
      let cl = List.length checklist in
      if cl != 0 then (fn + bn - cl, cl) else (bn, fn)
    in
    if nb > 0 then (
      Format.fprintf fmt "background-packages: %d@." nb ;
      Format.fprintf fmt "foreground-packages: %d@." nf) ;
    Format.fprintf fmt "total-packages: %d@." universe_size ;
    Format.fprintf fmt "broken-packages: %d@." nbp ;
    if summary then
      Format.fprintf
        fmt
        "@[%a@]@."
        (Diagnostic.pp_summary ~explain:explain_summary ~pp ())
        results ;
    nbp

;;
StdUtils.if_application
  ~alternatives:
    [ "debcheck";
      "dose-debcheck";
      "dose-distcheck";
      "eclipsecheck";
      "dose-eclipsecheck";
      "rpmcheck";
      "dose-rpmcheck" ]
  "distcheck"
  main
