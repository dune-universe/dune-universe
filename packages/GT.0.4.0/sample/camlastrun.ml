open Config
open Clflags
open Compenv

open Misc
open Format
open Typedtree

let tool_name = "ocamlhtml"

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  try
    let untyped =
      Pparse.parse_implementation ~tool_name ppf sourcefile
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
    in
    (* let () = Pprintast.structure Format.std_formatter untyped in *)

    let () =
      let ch = open_out "out.fmt.txt" in
      let fmt = Format.formatter_of_out_channel ch in
      Format.pp_set_margin fmt 180;
      Camlast.fmt_structure fmt untyped;
      Format.pp_print_flush fmt ();
      close_out ch
    in
    let () =
      let ch = open_out "out.html" in
      let fmt = Format.formatter_of_out_channel ch in
      Format.pp_set_margin fmt 180;
      Format.fprintf fmt "%s"
        {|
<head>
<SCRIPT SRC="mktree.js" LANGUAGE="JavaScript"></SCRIPT>
<LINK REL="stylesheet" HREF="mktree.css">
<script language="javascript">
document.addEventListener("DOMContentLoaded", function(event) { 
        document.body.firstChild.class = "mktree";
        convertTrees();
});
</script>
</head>
<body>
|};

      Format.fprintf fmt "%s" @@ HTML.toHTML @@
      HTML.body @@ Camlast.html_structure untyped;
      Format.fprintf fmt "%s" {|</body>|};

      Format.pp_print_flush fmt ();
      close_out ch
    in
    ()
  with
    | Typetexp.Error (_loc,env,e) as exc ->
      Typetexp.report_error env Format.std_formatter e;
      Format.printf "\n%!";
      raise exc
    (* | Misc.HookExnWrapper wr -> raise wr.Misc.error *)
    | x -> raise x

let process_implementation_file ppf name =
  let opref = output_prefix name in
  implementation ppf name opref;
  objfiles := (opref ^ ".cmo") :: !objfiles

let process_file ppf name =
  Printf.printf "process_file `%s`\n%!" name;
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    let opref = output_prefix name in
    implementation ppf name opref;
    objfiles := (opref ^ ".cmo") :: !objfiles
end else
    raise(Arg.Bad("don't know what to do with " ^ name))


let usage = "Usage: ocamlc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

let before_compile s =
  (* in 4.02 we ignore argument *)
  Before_compile s

(* Error messages to standard error formatter *)
let anonymous filename =
  readenv ppf (before_compile filename); process_file ppf filename;;
let impl filename =
  readenv ppf (before_compile filename); process_implementation_file ppf filename;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;


module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = show_config
  let _custom = set custom_runtime
  let _no_check_prims = set no_check_prims
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _keep_locs = set keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _no_alias_deps = set transparent_modules
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s =
    Printf.printf "setting output name '%s'\n" s;
    output_name := Some s
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _output_complete_obj () =
    output_c_object := true; output_complete_object := true; custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _plugin p = Compplugin.load p
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _strict_formats = set strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr

  let _opaque = set opaque
  let _no_principal = unset principal
  let _no_rectypes = unset recursive_types
  let _no_strict_sequence = unset strict_sequence
  let _no_strict_formats = unset strict_formats
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = unset unboxed_types
  let _color option =
    begin match parse_color_setting option with
          | None -> ()
          | Some setting -> color := Some setting
    end
  let _dtimings () = profile_columns := [ `Time ]
  let _dprofile () = profile_columns := Profile.all_columns
  let _alias_deps = unset transparent_modules
  let _app_funct = set applicative_functors
  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
  let _no_keep_docs = unset keep_docs
  let _no_keep_locs = unset keep_locs

  let anonymous = anonymous
end)

let () =
  readenv ppf Before_args;
  Arg.parse Options.list anonymous usage;
  ()
