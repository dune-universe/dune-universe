(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** {b Entrypoint for the 'electrod' program.} Performs CLI management.*)

open Stdcompat
open Containers
open Cmdliner
open Libelectrod

let natural_arg =
  let parse s =
    let errmsg = Error (`Msg "not a natural number") in
    match int_of_string_opt s with
    | None ->
        errmsg
    | Some n when n < 0 ->
        errmsg
    | Some n ->
        Ok n
  in
  Arg.conv (parse, Fmt.int)


let infile =
  let doc = "File to process." in
  Arg.(
    required
    & pos 0 (some ~none:"missing FILE" non_dir_file) None
    & info [] ~docv:"ELECTROD_FILE" ~doc)


let tool =
  let doc =
    {|Analysis tool to rely upon. $(docv) must be one of `nuXmv' or 
              `NuSMV'.|}
  in
  Arg.(
    value
    & opt (enum [ ("nuXmv", Main.NuXmv); ("NuSMV", Main.NuSMV) ]) Main.NuXmv
    & info [ "t"; "tool" ] ~docv:"TOOL" ~doc)


let bmc_length =
  let doc = {|Sets the bound on trace steps. $(docv) must be non-negative.|} in
  Arg.(value & opt (some natural_arg) None & info [ "bmc" ] ~docv:"BOUND" ~doc)


let script =
  let doc =
    {|Script to pass to the analysis tool (default: see section FILES). 
              A script file for nuXmv 
              or NuSMV $(b,MUST) set both options `default_trace_plugin' and 
              `on_failure_script_quits' to '1'. If the '--bmc' option is used, the BMC bound will be prepended to the script file; and the script file is expected to call a BMC algorithm.  (NOTICE: no well-formedness verification of the script file is made whatsoever).|}
  in
  Arg.(
    value
    & opt (some non_dir_file) None
    & info [ "s"; "script" ] ~docv:"SCRIPT_FILE" ~doc)


let keep_files =
  let doc =
    {|If present, keep the generated model and script files (in the same 
      directory as ELECTROD_FILE). |}
  in
  Arg.(value & flag & info [ "kg"; "keep-generated" ] ~doc)


let no_analysis =
  let doc =
    {|If present, do not perform the analysis (files are still generated).|}
  in
  Arg.(value & flag & info [ "na"; "no-analysis" ] ~doc)


let print_generated =
  let doc = {|If present, print the generated file on the standard output.|} in
  Arg.(value & flag & info [ "pg"; "print-generated" ] ~doc)


let long_names =
  let doc =
    {|If present, use long relation and atom names in the generated file.|}
  in
  Arg.(value & flag & info [ "ln"; "long-names" ] ~doc)


let outcome_format =
  let doc =
    {|Format for the outcome of analysis. 
      $(docv) must be one of `chrono', `plain' or `xml'|}
  in
  Arg.(
    value
    & opt
        (enum [ ("chrono", `Chrono); ("plain", `Plain); ("xml", `XML) ])
        `Chrono
    & info [ "of"; "outcome-format" ] ~docv:"FORMAT" ~doc)


let temporal_symmetry =
  let doc =
    {|If present, generate the full temporal symmetry breaking predicate. Otherwise a single-state symmetry breaking predicate is generated.|}
  in
  Arg.(value & flag & info [ "ts"; "temporal-symmetry" ] ~doc)


let symmetry_offset =
  let doc =
    {|Sets the offset of the single-state symmetry breaking predicate. Only taken into account if the option temporal-symmetry is NOT selected. $(docv) must be non-negative.|}
  in
  Arg.(
    value
    & opt natural_arg 0
    & info [ "so"; "symmetry-offset" ] ~docv:"SYMMETRY_OFFSET" ~doc)


(* verbosity options (already def'd in Logs_cli, thx!) *)
let verb_term = Logs_cli.level ()

(* same for colors *)
let color_term = Fmt_cli.style_renderer ()

let main_term =
  Term.(
    const Main.main
    $ color_term
    $ verb_term
    $ tool
    $ infile
    $ script
    $ keep_files
    $ no_analysis
    $ print_generated
    $ outcome_format
    $ long_names
    $ bmc_length
    $ temporal_symmetry
    $ symmetry_offset)


let main_info =
  let doc = "formal analysis of Electrod models" in
  let man =
    [ `S Manpage.s_files
    ; `P {|Default SCRIPT_FILE for unbounded model-checking with nuXmv:|}
    ; `Noblank
    ; `Pre Smv.nuXmv_default_script
    ; `Noblank
    ; `P {|Default SCRIPT_FILE for unbounded model-checking with NuSMV:|}
    ; `Noblank
    ; `Pre Smv.nuSMV_default_script
    ; `P {|Default SCRIPT_FILE for bounded model-checking with NuSMV:|}
    ; `Noblank
    ; `Pre Smv.nuSMV_default_bmc_script
    ; `P {|Default SCRIPT_FILE for bounded model-checking with nuXmv:|}
    ; `Noblank
    ; `Pre Smv.nuXmv_default_bmc_script
    ; `S Manpage.s_authors
    ; `P {|Julien BRUNEL (ONERA), David CHEMOUIL (ONERA).|}
    ; `S "COPYRIGHT"
    ; `P "Electrod (C) 2016-2020 ONERA."
    ; `P
        "Electrod is free software: you can redistribute it and/or modify it \
         under the terms of the Mozilla Public License, v. 2.0. If a copy of \
         the MPL was not distributed with this file, You can obtain one at \
         http://mozilla.org/MPL/2.0/."
    ; `P
        "Electrod is distributed in the hope that it will be useful, but \
         WITHOUT ANY WARRANTY; without even the implied warranty of \
         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. "
    ; `S "THIRD-PARTY SOFTWARE"
    ; `P
        {|Electrod relies on third-party free software, 
         please refer to the Electrod 
         OPAM repository for the full text of their licenses.|}
    ]
  in
  Term.info "electrod" ~doc ~man


let () =
  (* process commandline arguments and run the actual main (Main.main) function *)
  match Term.eval ~catch:true (main_term, main_info) with
  | `Error _ ->
      exit 1
  | _ ->
      exit (if Logs.err_count () > 0 then 1 else 0)
