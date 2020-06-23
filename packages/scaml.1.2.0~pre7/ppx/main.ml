(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2020  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Ppxlib
open Ocaml_common

let log fmt = Format.eprintf fmt

(* XXX Maybe not precise, but I do not know the proper way to get the source 
   code name *)
let get_source_of_str = function
  | [] -> None
  | s::_ -> Some s.pstr_loc.Location.loc_start.Lexing.pos_fname

(* modified version of Compile_common.with_info *)
let with_info str k =
  Compmisc.init_path ();
  let source_file = 
    match get_source_of_str str with
    | Some fn -> fn
    | None -> "noname"
  in
  let output_prefix = Compenv.output_prefix source_file in
  let module_name = Compenv.module_of_filename source_file output_prefix in
  Env.set_unit_name module_name;
  let env = Compmisc.initial_env () in
  let dump_file = String.concat "." [output_prefix; ".cmotmp"] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k { Compile_common.source_file
    ; module_name
    ; output_prefix
    ; env
    ; ppf_dump
    ; tool_name= "scaml.ppx"
    ; native= false
    }

(* returns [true] if [str] contains [scaml] attribute *)
let is_with_scaml str =
  let module M = struct
    class iter = object
      inherit Ast_traverse.iter
      method! attribute a =
        if a.attr_name.txt = "SCaml" then raise Exit
    end
  end
  in
  let i = new M.iter in
  try i#structure str; false with Exit -> true

open Spotlib.Spot

(* PPX only inherits the following Clflag options.  See parsing/ast_mapper.ml,
   PpxContext.make:

   lid "tool_name",    make_string tool_name;
   lid "include_dirs", make_list make_string !Clflags.include_dirs;
   lid "load_path",    make_list make_string (Load_path.get_paths ());
   lid "open_modules", make_list make_string !Clflags.open_modules;
   lid "for_package",  make_option make_string !Clflags.for_package;
   lid "debug",        make_bool !Clflags.debug;
   lid "use_threads",  make_bool !Clflags.use_threads;
   lid "use_vmthreads", make_bool false;
   lid "recursive_types", make_bool !Clflags.recursive_types;
   lid "principal", make_bool !Clflags.principal;
   lid "transparent_modules", make_bool !Clflags.transparent_modules;
   lid "unboxed_types", make_bool !Clflags.unboxed_types;
   lid "unsafe_string", make_bool !Clflags.unsafe_string;
*)
let preprocess str info =
  Clflags.dont_write_files := true;
  
  (* We need OCaml's str, not one for Ppxlib *)
  let str' = Ppxlib_ast.Selected_ast.to_ocaml Structure str in
  let typed = Compile_common.typecheck_impl info str' in
  let typed = 
    let str, coe = typed in 
    let str = SCamlc.Translate.filter_by_SCaml_attribute str in
    str, coe
  in
  let module_ = 
    (* Exceptions must be raised as are, to be handled nicely by
       ocamlc and merlin *)
    SCamlc.SCamlComp.compile_only
      info.source_file info.output_prefix info.module_name
      typed
  in
  (* We have to put the def NOT at the bottom but at the head,
     since it must precede the call of SCamlPPX.emit *)
  (* log "Adding IML to %s@." info.Compile_common.module_name; *)
  [ let open Ast_builder.Default in
    pstr_value ~loc:Location.none
      Nonrecursive
      [ value_binding ~loc:Location.none 
          ~pat:(punit ~loc:Location.none)
          ~expr:(eapply ~loc:Location.none
                   (evar ~loc:Location.none "SCamlc.Ppx.register")
                   [estring ~loc:Location.none 
                      (Marshal.to_string module_ [])])
      ]
    ]
  @ str 

let impl str =
  let tool_name = Ast_mapper.tool_name () in
  match tool_name with
  | "ocamldep" -> str
  | _ when not @@ is_with_scaml str -> str
  | "merlin" | "ocamlc" | "ocamlopt" ->
      SCamlc.Flags.if_debug (fun () -> 
          log "scaml.ppx: %s %s@." tool_name 
            (Stdlib.Option.value (get_source_of_str str) ~default:"???"));
      with_info str @@ preprocess str
  | _ -> 
      Format.eprintf "scaml.ppx: called from unknown tool %s.  Skip processing" tool_name;
      str

let () = Driver.register_transformation ~impl "scaml.ppx"
