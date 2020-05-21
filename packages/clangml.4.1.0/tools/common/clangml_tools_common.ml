type 'a options =
    string option -> string option -> string list -> bool -> bool ->'a

let option_language =
  let doc = "Language selection." in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["x"; "language"] ~docv:"LANG" ~doc)

let option_standard =
  let doc = "Standard selection." in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["std"] ~docv:"STD" ~doc)

let option_include_path =
  let doc = "Include search path." in
  Cmdliner.Arg.(
    value & opt_all dir [] & info ["I"; "include-path"] ~docv:"PATH" ~doc)

let option_include_clang =
  let doc = "Add Clang's include directory in search path." in
  Cmdliner.Arg.(
    value & flag & info ["i"; "include-clang"] ~doc)

let option_trigraphs =
  let doc = "Enable trigraphs." in
  Cmdliner.Arg.(
    value & flag & info ["trigraphs"] ~doc)

let options term =
  Cmdliner.Term.(term $ option_language $ option_standard $
    option_include_path $ option_include_clang $ option_trigraphs)

let command_line k language standard include_paths include_clang trigraphs =
  let command_line_args =
    List.map Clang.Command_line.include_directory include_paths in
  let command_line_args =
    match language with
    | None -> command_line_args
    | Some language ->
        Clang.Command_line.language_of_string language :: command_line_args in
  let command_line_args =
    match standard with
    | None -> command_line_args
    | Some standard ->
        Clang.Command_line.standard_of_string standard :: command_line_args in
  let command_line_args =
    if include_clang then
      List.map Clang.Command_line.include_directory
        (Clang.default_include_directories ()) @ command_line_args
    else
      command_line_args in
  let command_line_args =
    if trigraphs then
      Clang.Command_line.trigraphs :: command_line_args
    else
      command_line_args in
  k command_line_args
