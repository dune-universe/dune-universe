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
open Common
open Algo

module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description = "Compute the list broken packages in a repository"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  include Boilerplate.DistcheckOptions
  Boilerplate.DistcheckOptions.add_options options ;;

  include Boilerplate.InputOptions
  Boilerplate.InputOptions.add_options ~default:["checkonly";"outfile"] options ;;
(*
  add options ~short_name:'o' ~long_name:"outfile" ~help:"Output file" convtable;
*)
end

include Util.Logging(struct let label = "" end) ;;

let parse_tables filename =
  let ic = open_in filename in
  let l = ref [] in
  try
    begin while true do
      let line = input_line ic in
      Scanf.sscanf line "%s %s => %d" (fun p -> fun v -> fun i -> l := (p,v,i)::!l);
    done;
    assert false
    end
  with End_of_file -> (close_in ic; !l)
;;

let init_tables filename =
  let table = parse_tables filename in
  let tocudf = Hashtbl.create (2 * List.length table) in
  let fromcudf = Hashtbl.create (2 * List.length table) in
  List.iter (fun (p,v,i) ->
    Hashtbl.add tocudf (p,v) i;
    Hashtbl.add fromcudf (p,i) v
  ) table;
  (tocudf,fromcudf)
;;

let cudf_load_list table file =
  let preamble, pkglist =
    match Boilerplate.parse_cudf file with
    |None, pkglist, _ -> Cudf.default_preamble, pkglist
    |Some p , pkglist, _ -> p, pkglist
  in
  let tocudf,fromcudf = init_tables table in
  let from_cudf (p,i) = (p,try Hashtbl.find fromcudf (p,i) with Not_found -> string_of_int i) in
  let to_cudf (p,v) = (p,try Hashtbl.find tocudf (p,v) with Not_found -> int_of_string v) in
  (preamble,pkglist,from_cudf,to_cudf)
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let (preamble,pkgl,from_cudf,to_cudf) = cudf_load_list "aa" (List.hd posargs) in
  let universe = Cudf.load_universe pkgl in
  let universe_size = Cudf.universe_size universe in

  let checklist = 
    if OptParse.Opt.is_set Options.checkonly then begin
      info "--checkonly specified, consider all packages as background packages";
      List.flatten (
        List.map (fun ((n,a),c) ->
          let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else []
  in

  let pp = CudfAdd.pp from_cudf in

  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failure in
  let success = OptParse.Opt.get Options.success in
  let explain = OptParse.Opt.get Options.explain in
  let minimal = OptParse.Opt.get Options.minimal in
  let summary = OptParse.Opt.get Options.summary in
  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      let oc = open_out (OptParse.Opt.get Options.outfile) in
      Format.formatter_of_out_channel oc
    else
      Format.std_formatter
  in
  let results = Diagnostic.default_result universe_size in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d =
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~success ~explain ~minimal fmt d
  in

  begin 
    let global_constraints = false in
    let nbp =
      if OptParse.Opt.is_set Options.checkonly then 
        Depsolver.listcheck ~global_constraints ~callback universe checklist
      else
        Depsolver.univcheck ~global_constraints ~callback universe
    in
    
    if failure || success then Format.fprintf fmt "@]@.";
    
    Format.fprintf fmt "total-packages: %d@." universe_size;
    Format.fprintf fmt "broken-packages: %d@." nbp;
    if summary then 
      Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;
    Boilerplate.exit(nbp)
  end
;;

Boilerplate.if_application ~alternatives:[ "dormcheck" ] "" main ;;

