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

open ExtLib
open Dose_common
open Dose_algo
open Dose_debian
open Dose_doseparse

module Options = struct
  open OptParse

  let description =
    "Check for Debian Package Coinstallability."
    ^ "Return the list of binary package. If --src <Sources> is specified"
    ^ "then we return the list of corresponding source packages"

  let options = OptParser.make ~description

  include StdOptions.MakeOptions (struct
    let options = options
  end)

  let sources = StdOpt.str_option ()

  let dump = StdOpt.str_option ()

  let tupletable = StdOpt.str_option ()

  let cputable = StdOpt.str_option ()

  include StdOptions.DistcheckOptions;;

  let default =
    List.remove StdOptions.DistcheckOptions.default_options "successes"
  in
  StdOptions.DistcheckOptions.add_options ~default options

  include StdOptions.InputOptions;;

  let default =
    List.fold_left
      (fun acc e -> List.remove acc e)
      StdOptions.InputOptions.default_options
      ["inputtype"; "compare"]
  in
  StdOptions.InputOptions.add_options ~default options ;
  StdOptions.InputOptions.add_option
    options
    ~long_name:"src"
    ~help:"Associate Sources file"
    sources

  include StdOptions.OutputOptions;;

  StdOptions.OutputOptions.add_options options ;
  StdOptions.OutputOptions.add_option
    options
    ~long_name:"dump"
    ~help:"dump the cudf file"
    dump

  include StdOptions.DistribOptions;;

  let default =
    List.fold_left
      (fun acc e -> List.remove acc e)
      StdOptions.DistribOptions.default_options
      [ "deb-host-arch";
        "deb-drop-b-d-indep";
        "deb-drop-b-d-arch";
        "deb-profiles" ]
  in
  StdOptions.DistribOptions.add_debian_options ~default options ;
  StdOptions.DistribOptions.add_option
    options
    ~long_name:"deb-tupletable"
    ~help:"Path to an architecture tuple table like /usr/share/dpkg/tupletable"
    tupletable ;
  StdOptions.DistribOptions.add_option
    options
    ~long_name:"deb-cputable"
    ~help:"Path to a cpu table like /usr/share/dpkg/cputable"
    cputable
end

include Util.Logging (struct
  let label = "dose_applications.debCoinstall"
end)

let timer = Util.Timer.create "Solver"

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  StdDebug.enable_debug (OptParse.Opt.get Options.verbose) ;
  StdDebug.enable_timers (OptParse.Opt.get Options.timers) ["Solver"; "Load"] ;
  StdDebug.enable_bars
    (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck"; "Depsolver_int.init_solver"] ;
  StdDebug.all_quiet (OptParse.Opt.get Options.quiet) ;
  let options = Options.set_deb_options () in
  let (fg, bg) = Options.parse_cmdline (`Deb, false) posargs in
  (if
   (OptParse.Opt.is_set Options.tupletable
   || OptParse.Opt.is_set Options.cputable)
   && OptParse.Opt.is_set Options.sources
  then
   let ttfile =
     if OptParse.Opt.is_set Options.tupletable then
       Some (OptParse.Opt.get Options.tupletable)
     else None
   in
   let ctfile =
     if OptParse.Opt.is_set Options.cputable then
       Some (OptParse.Opt.get Options.cputable)
     else None
   in
   Architecture.read_tupletable ~ttfile ~ctfile ()) ;
  let cudftodeb_table = Hashtbl.create 30000 in
  let cudftosrc_table = Hashtbl.create 30000 in
  let deb_load_list options ?(status = []) sources urilist =
    let native = Option.get options.Dose_debian.Debcudf.native in
    let archs =
      if native <> "" then native :: options.Dose_debian.Debcudf.foreign else []
    in
    let dll =
      List.map
        (fun filelist -> Dose_debian.Packages.input_raw ~archs filelist)
        urilist
    in
    let pkglist = List.flatten dll in
    let pkglist =
      if status = [] then pkglist else Dose_debian.Packages.merge status pkglist
    in
    let origsourcelist =
      if not (Option.is_none sources) then
        Sources.input_raw ~archs [Option.get sources]
      else []
    in
    let srclist =
      if not (Option.is_none sources) then
        Sources.sources2packages
          ~noindep:true
          ~noarch:false
          ~profiles:[]
          native
          native
          origsourcelist
      else []
    in
    let tables = Dose_debian.Debcudf.init_tables ~options (srclist @ pkglist) in
    let from_cudf (p, i) = Dose_debian.Debcudf.get_real_version tables (p, i) in
    let to_cudf (p, v) =
      (p, Dose_debian.Debcudf.get_cudf_version tables (p, v))
    in
    let srcl =
      let dl = List.map (Dose_debian.Debcudf.tocudf tables ~options) srclist in
      List.iter2
        (fun cudfpkg srcpkg ->
          let id = (cudfpkg.Cudf.package, cudfpkg.Cudf.version) in
          Hashtbl.add cudftosrc_table id srcpkg)
        dl
        origsourcelist ;
      dl
    in
    let cll =
      List.map
        (fun l ->
          List.map
            (fun debpkg ->
              let cudfpkg = Dose_debian.Debcudf.tocudf tables ~options debpkg in
              let id = (cudfpkg.Cudf.package, cudfpkg.Cudf.version) in
              Hashtbl.add cudftodeb_table id debpkg ;
              cudfpkg)
            (Dose_debian.Packages.merge status l))
        dll
    in
    let preamble = Dose_debian.Debcudf.preamble in
    let global_constraints =
      Dose_debian.Debcudf.get_essential ~options tables
    in
    (preamble, cll, srcl, from_cudf, to_cudf, global_constraints)
  in
  let sources = OptParse.Opt.opt Options.sources in
  let (_preamble, pkgll, srclist, from_cudf, to_cudf, global_constraints) =
    deb_load_list options sources [fg; bg]
  in
  let (fg_pkglist, bg_pkglist) =
    match pkgll with [fg; bg] -> (fg, bg) | _ -> assert false
  in
  let fg_pkglist =
    if OptParse.Opt.is_set Options.latest then
      CudfAdd.latest ~n:(OptParse.Opt.get Options.latest) fg_pkglist
    else fg_pkglist
  in
  let universe =
    let s = CudfAdd.to_set (srclist @ fg_pkglist @ bg_pkglist) in
    Cudf.load_universe (CudfAdd.Cudf_set.elements s)
  in
  let universe_size = Cudf.universe_size universe in
  if OptParse.Opt.is_set Options.dump then (
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file" ;
    Cudf_printer.pp_preamble oc Debcudf.preamble ;
    Printf.fprintf oc "\n" ;
    Cudf_printer.pp_universe oc universe) ;
  let checklist =
    if OptParse.Opt.is_set Options.checkonly then (
      info "--checkonly specified, consider all packages as background packages" ;
      List.flatten
        (List.map
           (fun ((n, a), c) ->
             let (name, filter) =
               Dose_pef.Pefcudf.pefvpkg to_cudf ((n, a), c)
             in
             Cudf.lookup_packages ~filter universe name)
           (OptParse.Opt.get Options.checkonly)))
    else fg_pkglist
  in
  let pp = CudfAdd.pp from_cudf in
  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failure in
  let explain = OptParse.Opt.get Options.explain in
  let minimal = OptParse.Opt.get Options.minimal in
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else stdout
  in
  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      Format.formatter_of_out_channel oc
    else Format.std_formatter
  in
  Util.Timer.start timer ;
  let result =
    Depsolver.edos_coinstall ~global_constraints universe checklist
  in
  ignore (Util.Timer.stop timer ()) ;
  let exitcode =
    if Diagnostic.is_solution result then (
      let is = Diagnostic.get_installationset result in
      (if Option.is_none sources then
       List.iter
         (fun cudfpkg ->
           try
             let id = (cudfpkg.Cudf.package, cudfpkg.Cudf.version) in
             let debpkg = Hashtbl.find cudftodeb_table id in
             debpkg#pp oc
           with Not_found -> assert false)
         is
      else
        let l =
          List.unique
            (List.map
               (fun binpkg ->
                 let cudfpkg = Sources.get_src_package universe binpkg in
                 let id = (cudfpkg.Cudf.package, cudfpkg.Cudf.version) in
                 Hashtbl.find cudftosrc_table id)
               is)
        in
        List.iter
          (fun pkg ->
            pkg#pp oc ;
            Printf.fprintf oc "\n")
          l) ;
      0 (* exit code 0 . All packages are installable *))
    else (
      if failure then (
        Diagnostic.pp_out_version fmt ;
        Format.fprintf fmt "@[<v 1>report:@," ;
        Diagnostic.fprintf ~pp ~minimal ~failure ~explain fmt result ;
        Format.fprintf fmt "@]@." ;
        let fn = List.length fg_pkglist in
        let bn = List.length bg_pkglist in
        let (nb, nf) =
          let cl = List.length checklist in
          if cl != 0 then (fn + bn - cl, cl) else (bn, fn)
        in
        Format.fprintf fmt "background-packages: %d@." nb ;
        Format.fprintf fmt "foreground-packages: %d@." nf ;
        Format.fprintf fmt "total-packages: %d@." universe_size) ;
      1 (* at least one package is not installable *))
  in
  exitcode
;;

StdUtils.if_application
  ~alternatives:["dose-deb-coinstall"; "deb-coinstall"]
  "debCoinstall"
  main
