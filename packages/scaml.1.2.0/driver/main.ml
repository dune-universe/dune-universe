(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open SCamlc

module Compile_common = struct
open Misc
open Compenv

type info = {
  source_file : string;
  module_name : string;
  output_prefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let cmx i = i.output_prefix ^ ".cmx"
let obj i = i.output_prefix ^ Config.ext_obj
let cmo i = i.output_prefix ^ ".cmo"
let annot i = i.output_prefix ^ ".annot"

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  Compmisc.init_path ();
  let module_name = module_of_filename source_file output_prefix in
  Env.set_unit_name module_name;
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k {
    module_name;
    output_prefix;
    env;
    source_file;
    ppf_dump;
    tool_name;
    native;
  }

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let tsg =
    ast
    |> Typemod.type_interface info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature info.source_file)
          sg);
  ignore (Includemod.signatures info.env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

let emit_signature info ast tsg =
  let sg =
    let alerts = Builtin_attributes.alerts_of_sig ast in
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      info.module_name (info.output_prefix ^ ".cmi")
  in
  Typemod.save_signature info.module_name tsg
    info.output_prefix info.source_file info.env sg

let interface info =
  Profile.record_call info.source_file @@ fun () ->
  let ast = parse_intf info in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tsg = typecheck_intf info ast in
    if not !Clflags.print_types then begin
      emit_signature info ast tsg
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  let always () = Stypes.dump (Some (annot i)) in
  Misc.try_finally ~always (fun () ->
    parsetree
    |> Profile.(record typing)
      (Typemod.type_implementation
         i.source_file i.output_prefix i.module_name i.env)
    |> print_if i.ppf_dump Clflags.dump_typedtree
      Printtyped.implementation_with_coercion
  )

let implementation info ~backend =
  Profile.record_call info.source_file @@ fun () ->
  let exceptionally () =
    let sufs = if info.native then [ cmx; obj ] else [ cmo ] in
    List.iter (fun suf -> remove_file (suf info)) sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parsed = parse_impl info in

    let m = SCamlc.Preprocess.mapper in
    let parsed = m.Ast_mapper.structure m parsed in

    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl info parsed in
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed
      end;
    end;
    Warnings.check_fatal ();
  )
end

module Compile = struct
open Compile_common

let tool_name = "scamlc"

let with_info =
  Compile_common.with_info ~native:false ~tool_name

let interface ~source_file:_ ~output_prefix:_ = assert false
(*
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info
*)

let implementation ~source_file ~output_prefix =
  let backend i typed = SCamlc.SCamlComp.compile i.source_file i.output_prefix i.module_name typed in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
  Compile_common.implementation info ~backend
end

open Clflags
open Compenv

let usage = "Usage: scamlc <options> <files>\nOptions are:"

(* Error messages to standard error formatter *)
let ppf = Format.err_formatter

let vmthread_removed_message = "\
The -vmthread argument of ocamlc is no longer supported\n\
since OCaml 4.09.0.  Please switch to system threads, which have the\n\
same API. Lightweight threads with VM-level scheduling are provided by\n\
third-party libraries such as Lwt, but with a different API."

module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Clflags.absname
  let _alert = Warnings.parse_alert_option
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = Compenv.defer (ProcessObjects (Misc.rev_split_words s))
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = Misc.show_config_and_exit
  let _config_var = Misc.show_config_variable_and_exit
  let _custom = set custom_runtime
  let _no_check_prims = set no_check_prims
  let _dllib s = defer (ProcessDLLs (Misc.rev_split_words s))
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () =
    print_types := true;
    compile_only := true;
    stop_after := Some Compiler_pass.Typing;
    ()
  let _stop_after pass =
    let module P = Compiler_pass in
    begin match P.of_string pass with
    | None -> () (* this should not occur as we use Arg.Symbol *)
    | Some pass ->
        stop_after := Some pass;
        begin match pass with
        | P.Parsing | P.Typing ->
            compile_only := true
        end;
    end
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _no_keep_docs = unset keep_docs
  let _keep_locs = set keep_locs
  let _no_keep_locs = unset keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _alias_deps = unset transparent_modules
  let _no_alias_deps = set transparent_modules
  let _app_funct = set applicative_functors
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _opaque = set opaque
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _output_complete_obj () =
    output_c_object := true;
    output_complete_object := true;
    custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _plugin _p = plugin := true
  let _principal = set principal
  let _no_principal = unset principal
  let _rectypes = set recursive_types
  let _no_rectypes = unset recursive_types
  let _runtime_variant s = runtime_variant := s
  let _with_runtime = set with_runtime
  let _without_runtime = unset with_runtime
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = unset strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = unset strict_formats
  let _thread = set use_threads
  let _vmthread = fun () -> fatal vmthread_removed_message
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = unset unboxed_types
  let _unsafe = set unsafe
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _color = Misc.set_or_ignore color_reader.parse color
  let _error_style = Misc.set_or_ignore error_style_reader.parse error_style
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _match_context_rows n = match_context_rows := n
  let _dump_into_file = set dump_into_file
  let _dno_unique_ids = unset unique_ids
  let _dunique_ids = set unique_ids
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let _dcamlprimc = set keep_camlprimc_file
  let _dtimings () = profile_columns := [ `Time ]
  let _dprofile () = profile_columns := Profile.all_columns

  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0

  let anonymous = anonymous
end)

let scaml_print_version_and_library compiler =
  Printf.printf "The SCaml %s, version %s for Tezos protocol version %s"
    compiler Version.scaml Version.protocol; print_newline();
  Printf.printf "SCaml library directory: ";
  print_string begin match !SCamlc.SCamlComp.scamlib with None -> "none" | Some d -> d end; print_newline ();
  Printf.printf "The OCaml %s, version " compiler;
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let scaml_print_version_string () =
  print_string Config.version; print_newline(); exit 0

let main () =
  let opt = ref Conf.none in
  Clflags.add_arguments __LOC__ Options.list;
  Clflags.add_arguments __LOC__
    ["-depend", Arg.Unit Makedepend.main_from_option,
     "<options> Compute dependencies (use 'ocamlc -depend -help' for details)"

    (* SCaml *)
    ; "--scaml-debug", Arg.Unit (fun () -> Conf.(opt := { !opt with op_debug = Some true })),
      "Print SCaml debug messages"
    ; "--scaml-time", Arg.Unit (fun () -> Conf.(opt := { !opt with op_time = Some true })),
      "Time SCaml compilation phases"
    ; "--scaml-convert", Arg.Unit (fun () -> Conf.(opt := { !opt with op_mode= Some ConvertAll })),
      "Convert types and values, instead of compling a smart contract"
    ; "--scaml-convert-value", Arg.String (fun s -> Conf.(opt := { !opt with op_mode= Some (ConvertSingleValue s) })),
      "<ident> Convert a single value, instead of compling a smart contract"
    ; "--scaml-convert-type", Arg.String (fun s -> Conf.(opt := { !opt with op_mode= Some (ConvertSingleType s)})),
      "<ident> Convert a single type, instead of compling a smart contract"
    ; "--scaml-revert", Arg.String (fun s -> Conf.(opt := { !opt with op_mode= Some (Revert s)})),
      "<string> Revert values, instead of compling a smart contract"
    ; "--scaml-noscamlib", Arg.Unit (fun () -> Conf.(opt := { !opt with op_noscamlib = Some true })),
      "Do not add default directory for SCamlib to the list of include directories"
    ; "--scaml-version", Arg.Unit (fun () ->
          print_string Version.scaml; print_newline(); exit 0),
      "Print SCaml version and exit"
    ; "--scaml-dump-iml", Arg.Unit (fun () -> Conf.(opt := { !opt with op_dump_iml = Some true })),
      "Dump the final IML code to .iml file"
    ; "--scaml-optimize", Arg.Bool (fun b -> Conf.(opt := { !opt with op_iml_optimization = Some b })),
      "<bool> Turn on/off the optimization.  Default is true.  False may break compilation."
    ; "--scaml-protocol", Arg.String (fun s ->
          match Protocol.parse s with
          | Ok v -> Conf.(opt := { !opt with op_protocol = Some v })
          | Error s -> failwith s
        ),
      (Printf.sprintf "<string> Set Tezos protocol version (default: %s)"
         Protocol.(to_string default))
    ];
  try
    readenv ppf Before_args;
    Clflags.parse_arguments anonymous usage;
    Conf.with_opt !opt @@ fun () ->
    SCamlc.SCamlComp.init ();
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      fatal "-plugin is only supported up to OCaml 4.08.0";
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Compile.implementation,
         Compile.interface,
         ".cmo",
         ".cma");
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2
      end
    end;
    readenv ppf Before_link;
    if
      List.length
        (List.filter (fun x -> !x)
           [make_archive;make_package;compile_only;output_c_object])
        > 1
    then begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
        fatal "Please specify at most one of -pack, -a, -c, -output-obj";
      | Some (P.Parsing | P.Typing) ->
          Printf.ksprintf fatal
            "Options -i and -stop-after (%s)\
             are  incompatible with -pack, -a, -output-obj"
            (String.concat "|" P.pass_names)
    end;
    if !make_archive then begin
      assert false
    end
    else if !make_package then begin
      assert false
    end
    else if not !compile_only && !objfiles <> [] then begin
      let compiled = List.rev !SCamlc.SCamlComp.rev_compiled in
      (* --scaml-convert and --scaml-revert do not produce compiled modules *)
      if compiled <> [] then begin
        Format.eprintf "Linking %s@." (String.concat ", " (List.map (fun x -> x.SCamlc.SCamlComp.Module.name) compiled));
        SCamlc.SCamlComp.link None (List.rev !SCamlc.SCamlComp.rev_compiled);
      end else begin
        Format.eprintf "Nothing to link...@."
      end;
      Warnings.check_fatal ();
    end;
  with x ->
    Location.report_exception ppf x;
    exit 2

let () =
  main ();
  Profile.print Format.std_formatter !Clflags.profile_columns;
  exit 0
