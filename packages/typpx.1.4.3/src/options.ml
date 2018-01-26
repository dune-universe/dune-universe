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

open Clflags
open Compenv

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
  let _cclib s = Compenv.defer (ProcessObjects (Misc.rev_split_words s))
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = show_config
  let _custom = set custom_runtime
  let _no_check_prims = set no_check_prims
  let _dllib s = defer (ProcessDLLs (Misc.rev_split_words s))
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
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
  (* let _plugin p = Compplugin.load p *)
  let _plugin _p = assert false
  let _principal = set principal
  let _no_principal = unset principal
  let _rectypes = set recursive_types
  let _no_rectypes = unset recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = unset strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = unset strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = unset unboxed_types
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
  let _color option =
    begin match parse_color_setting option with
          | None -> ()
          | Some setting -> color := Some setting
    end
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let _dtimings () = assert false
  let _dprofile () = assert false

  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0

  let anonymous = anonymous
end)

let compiler_libs_options =
  let inappropriate =
    [ "-a"
    ; "-c"
    ; "-cc"
    ; "-cclib"
    ; "-ccopt"
    ; "-compat-32"
    ; "-custom"
    ; "-dllib"
    ; "-dllpath"
    ; "-for-pack"
    ; "-g"
    (* ; "-i" *)
    ; "-linkall"
    ; "-make-runtime"
    ; "-make_runtime"
    ; "-noassert"
    ; "-noautolink"
    ; "-o"
    ; "-output-obj"
    ; "-pack"
    ; "-pp"
    ; "-ppx"
    ; "-runtime-variant"
    ; "-use-runtime"
    ; "-use_runtime"
    ; "-where"
    ; "-"
    ; "-nopervasives"
    ; "-use-prims"
    ; "-drawlambda"
    ; "-dlambda"
    ; "-dinstr"
    ; "-dtimings"
    ; "-dprofile"
    ]
  in
  List.filter (fun (n,_,_) -> not @@ List.mem n inappropriate) Options.list

