(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)

(** CLI to parse ProScript source code into OCaml. *)

let usage_msg =
  "dirsp-ps2ocaml <proscript_file>\n\
  \  [-s <ocaml_shims_module>]\n\
  \  [-i <ocaml_interface_module>]\n\
  \  (-p [<module_name>.]<parameter_name>:<parameter_type>)*\n\
  \  [-disable_strictequals_assertions]\n\
  \  [-apachev2license <YYYY Owner>]\n\
  \  (-whitelist_module_for_letin <module_name>)*\n\
  \  -o <ocaml_output_file>"


let disable_strictequals_assertions = ref false

let input_file = ref ""

let types_module_arg = ref ""

let shims_module_arg = ref ""

let output_file = ref ""

let header = ref ""

let parameter_types :
    (string * Dirsp_ps2ocamlcore.parameter_replacement) list ref =
  ref []


let whitelisted_letin_modules : string list ref = ref []

(** Adds <parameter_name>:<parameter_type> to the front of the parameter_types ref list. *)
let add_parameter_typing s =
  match String.split_on_char ':' s with
  | [ pname; ptype ] ->
      let parameter_name_and_replacement =
        match String.split_on_char '.' pname with
        | [ modulename; parametername ] ->
            ( parametername
            , Dirsp_ps2ocamlcore.
                { replacement_module_name = Some modulename
                ; replacement_type = ptype
                } )
        | [ parametername ] ->
            ( parametername
            , Dirsp_ps2ocamlcore.
                { replacement_module_name = None; replacement_type = ptype } )
        | _ ->
            failwith
              "The -p option must be -p \
               [<module_name>.]<parameter_name>:<parameter_type>"
      in
      parameter_types := parameter_name_and_replacement :: !parameter_types
  | _ ->
      failwith
        "The -p option must be -p \
         [<module_name>.]<parameter_name>:<parameter_type>"


let add_whitelisted_letin_module s =
  whitelisted_letin_modules := s :: !whitelisted_letin_modules


let add_apachev2license s =
  header :=
    !header
    ^ "(* Copyright "
    ^ s
    ^ "\n\n\
      \   Licensed under the Apache License, Version 2.0 (the \"License\");\n\
      \   you may not use this file except in compliance with the License.\n\
      \   You may obtain a copy of the License at\n\n\
      \       http://www.apache.org/licenses/LICENSE-2.0\n\n\
      \   Unless required by applicable law or agreed to in writing, software\n\
      \   distributed under the License is distributed on an \"AS IS\" BASIS,\n\
      \   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or \
       implied.\n\
      \   See the License for the specific language governing permissions and\n\
      \   limitations under the License. *)\n"


let anon_fun filename = input_file := filename

let speclist =
  [ ("-o", Arg.Set_string output_file, "Set output OCaml filename")
  ; ( "-i"
    , Arg.Set_string types_module_arg
    , "Set the OCaml interface module that will be included in the generated \
       output file. Defaults to the concatenation of a) capitalized basename \
       without extension of the output filename and b) the suffix '_intf', if \
       and only if the output filename is specified.\n\
       The interface module is _necessary_ to define any record types \
       implicitly defined by the ProScript; be sure to assign 'mutable' to \
       fields that change in ProScript" )
  ; ( "-s"
    , Arg.Set_string shims_module_arg
    , "Set the OCaml shims module that will be included in the generated \
       output file. Defaults to the concatenation of a) capitalized basename \
       without extension of the output filename and b) the suffix '_shims', if \
       and only if the output filename is specified.\n\
       The shims module is included so you can define your own OCaml functions \
       that could not be machine translated from ProScript, if any" )
  ; ( "-p"
    , Arg.String add_parameter_typing
    , "Annotate all function parameters with the given <parameter_name> to \
       have the <parameter_type> annotation. For the parameter type, you can \
       use a native type like 'string', a type you define in the OCaml \
       interface module, or a module qualified type like \
       'Bigarray.int16_unsigned_elt'. Can be specified multiple times" )
  ; ( "-whitelist_module_for_letin"
    , Arg.String add_whitelisted_letin_module
    , "Forces all the functions of the specified module to be machine \
       translated in let-in style. Since auto-detecting whether the let-in \
       style is sometimes too conservative, ps2ocaml will produce a warning in \
       the code (which will fail compilation). In the warning ps2ocaml will \
       present you with what you can do, and one of the things you can do is \
       enable this CLI option. The generated code will CHANGE when you enable \
       this option, so you must review the problem spots identified by new \
       'AUDIT NOTICE's. Can be specified multiple times" )
  ; ( "-disable_strictequals_assertions"
    , Arg.Set disable_strictequals_assertions
    , "If specified, convert any standalone equality statements like 'a.valid \
       === true;' into statements that only constrain the type but are \
       effectively no-ops. The default is to check for equality and raise an \
       Invalid_argument exception when the equality fails" )
  ; ( "-add_apachev2license"
    , Arg.String add_apachev2license
    , "Add an Apache v2 license to the generated file" )
  ]


let () =
  Arg.parse speclist anon_fun usage_msg ;
  let types_module =
    if !types_module_arg <> ""
    then !types_module_arg
    else if !output_file <> ""
    then
      StringLabels.capitalize_ascii
        (Filename.remove_extension (Filename.basename !output_file))
      ^ "_intf"
    else ""
  in
  let shim_module =
    if !shims_module_arg <> ""
    then !shims_module_arg
    else if !output_file <> ""
    then
      StringLabels.capitalize_ascii
        (Filename.remove_extension (Filename.basename !output_file))
      ^ "_shims"
    else ""
  in
  let error_msg =
    if !input_file = ""
    then Some "Missing input file(s)"
    else if shim_module = ""
    then
      Some
        "One or both of the output filename and interface module options must \
         be specified"
    else None
  in
  let p_opts = Dirsp_ps2ocamlcore.init_parsing_options () in
  let (t_opts : Dirsp_ps2ocamlcore.translation_options) =
    { treat_equality_as_assertion = not !disable_strictequals_assertions
    ; parameter_types = !parameter_types
    ; whitelisted_letin_modules = !whitelisted_letin_modules
    }
  in
  match error_msg with
  | None ->
      Dirsp_ps2ocamlcore.parse_and_translate
        !input_file
        !output_file
        !header
        types_module
        shim_module
        p_opts
        t_opts ;
      print_string "Finished auto-translation." ;
      print_newline ()
  | Some error_text ->
      print_string usage_msg ;
      Arg.usage speclist error_text ;
      print_newline ()
