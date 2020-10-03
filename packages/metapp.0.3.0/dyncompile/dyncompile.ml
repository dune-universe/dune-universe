module Options = Options

let output_structure (channel : out_channel) (s : Parsetree.structure) =
  let fmt = Format.formatter_of_out_channel channel in
  Pprintast.structure fmt s;
  Format.pp_print_flush fmt ()

type compiler = {
    command : string;
    archive_option : string;
    archive_suffix : string;
  }

let compiler : compiler =
  if Dynlink.is_native then {
    command = "ocamlopt";
    archive_option = "-shared";
    archive_suffix = ".cmxs";
  }
  else {
    command = "ocamlc";
    archive_option = "-a";
    archive_suffix = ".cma";
  }

let rec try_commands ~verbose list =
  match list with
  | [] -> assert false
  | (command, args) :: tl ->
      let command_line = Filename.quote_command command args in
      if verbose then
        prerr_endline command_line;
      match Sys.command command_line with
      | 0 -> ()
      | 127 when tl <> [] -> try_commands ~verbose tl
      | exit_code ->
          Location.raise_errorf ~loc:!Ast_helper.default_loc
            "@[Unable@ to@ compile@ preprocessor:@ command-line@ \"%s\"@ \
              failed@ with@ exit-code@ %d@]@."
            (String.escaped command_line) exit_code

let compile (options : Options.t) (source_filename : string)
    (object_filename : string) : unit =
  let flags =
    options.flags @
    List.concat_map (fun directory -> ["-I"; directory])
      options.directories @
    ["-I"; "+compiler-libs"; "-w"; "-40"; compiler.archive_option;
      source_filename; "-o"; object_filename] in
  let preutils_cmi = "metapp_preutils.cmi" in
  let api_cmi = "metapp_api.cmi" in
  let dune_preutils_path = "preutils/.metapp_preutils.objs/byte/" in
  let dune_api_path = "api/.metapp_api.objs/byte/" in
  let (flags, packages) =
    if Sys.file_exists preutils_cmi && Sys.file_exists api_cmi then
      (flags, options.packages)
    else if Sys.file_exists (Filename.concat dune_preutils_path preutils_cmi) &&
      Sys.file_exists (Filename.concat dune_api_path api_cmi) then
      (["-I"; dune_preutils_path; "-I"; dune_api_path] @ flags,
        options.packages)
    else
      (flags, ["metapp.preutils"; "metapp.api"] @ options.packages) in
  let commands =
    match packages with
    | [] ->
        [(compiler.command ^ ".opt", flags); (compiler.command, flags)]
    | _ ->
        [("ocamlfind",
          [compiler.command; "-package"; String.concat "," packages] @
          flags)] in
  try_commands ~verbose:options.verbose commands

(* Code taken from pparse.ml (adapted for a channel instead of a filename to use
   open_temp_file), because Pparse.write_ast is introduced in OCaml 4.04.0. *)
let write_ast (plainsource : bool) (channel : out_channel)
    (structure : Parsetree.structure) : unit =
  if plainsource then
    output_structure channel structure
  else
    begin
      output_string channel Config.ast_impl_magic_number;
      output_value channel !Location.input_name;
      output_value channel structure
    end

let compile_and_load (options : Options.t) (structure : Parsetree.structure)
  : unit =
  let (source_filename, channel) = Filename.open_temp_file "metapp" ".ml" in
  Fun.protect (fun () ->
    Fun.protect (fun () ->
      write_ast options.plainsource channel structure)
      ~finally:(fun () -> close_out channel);
    let object_filename =
      Filename.remove_extension source_filename ^
      compiler.archive_suffix in
    compile options source_filename object_filename;
    Fun.protect (fun () -> Dynlink.loadfile object_filename)
      ~finally:(fun () -> Sys.remove object_filename))
    ~finally:(fun () -> Sys.remove source_filename)
