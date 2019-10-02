let set_filename lexbuf filename =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename }

type snippet = Contents of string | File of string

let output_snippet out_channel snippet =
  match snippet with
  | Contents s ->
      output_string out_channel s
  | File filename ->
      let in_channel = open_in filename in
      Redirect.read_and_close in_channel begin fun () ->
        Redirect.output_channel_to_the_end out_channel in_channel
      end

type snippets = {
    before : snippet option;
    after : snippet option;
  }

let pos_column { Lexing.pos_cnum; pos_bol } =
  pos_cnum - pos_bol

let output_range out_channel { Lexer.start_pos; end_pos } =
  assert (start_pos.pos_fname = end_pos.pos_fname);
  if start_pos.pos_lnum = end_pos.pos_lnum then
    Printf.fprintf stderr "%s:%d:%d-%d" start_pos.pos_fname
      start_pos.pos_lnum (pos_column start_pos) (pos_column end_pos)
  else
    Printf.fprintf stderr "%s:%d:%d-%d:%d" start_pos.pos_fname
      start_pos.pos_lnum (pos_column start_pos)
      end_pos.pos_lnum (pos_column end_pos)

let extract_doc_channel ~filename snippets in_channel
    out_channel =
  let lexbuf = Lexing.from_channel in_channel in
  set_filename lexbuf filename;
  let context = {
    Lexer.out_channel;
    delimiter_stack = Stack.create ();
    warnings = Queue.create ();
    important_warnings = false
  } in
  Option.iter (output_snippet out_channel) snippets.before;
  Lexer.main context lexbuf;
  Option.iter (output_snippet out_channel) snippets.after;
  if not (Stack.is_empty context.delimiter_stack) then
    Lexer.mismatched_delimiters context
      (Stack.top context.delimiter_stack)
      lexbuf.lex_curr_p;
  if context.important_warnings then
    context.warnings |> Queue.iter @@ fun (range, message) ->
      Printf.fprintf stderr "%a:\nwarning: %s\n\n" output_range range message

let extract_doc_file_to_channel snippets ~source out_channel =
  let in_channel = open_in source in
  Fun.protect begin fun () ->
    extract_doc_channel ~filename:source snippets in_channel out_channel
  end
  ~finally:begin fun () ->
    close_in in_channel
  end

let extract_doc_file snippets ~source ~target =
  let out_channel = open_out target in
  Fun.protect begin fun () ->
    extract_doc_file_to_channel snippets ~source out_channel
  end
  ~finally:begin fun () ->
    close_out out_channel
  end

let extract_multiple_doc_files snippets files out_channel =
  files |> List.iter @@ fun source ->
    extract_doc_file_to_channel snippets ~source out_channel

let make_snippet ~contents ~file ~error =
  match contents, file with
  | None, None -> None
  | Some contents, None -> Some (Contents contents)
  | None, Some file -> Some (File file)
  | Some _, Some _ -> failwith error

let main target before before_file after after_file files =
  try
    let snippets = {
      before = make_snippet ~contents:before ~file:before_file ~error:
        "There should be at most one '--before' or one '--before-file' option";
      after = make_snippet ~contents:after ~file:after_file ~error:
        "There should be at most one '--after' or one '--after-file' option";
    } in
    match String.split_on_char '%' target, files with
    | _ :: _ :: _ :: _, _ ->
        failwith "There should be at most one '%' sign in target filename"
    | _, [] -> extract_doc_channel ~filename:"stdin" snippets stdin stdout
    | ["-"], _ -> extract_multiple_doc_files snippets files stdout
    | [target], [source] -> extract_doc_file snippets ~source ~target
    | [target], _ ->
        let out_channel = open_out target in
        Fun.protect begin fun () ->
          extract_multiple_doc_files snippets files out_channel 
        end
        ~finally:begin fun () ->
          close_out out_channel
        end
    | [prefix; suffix], _ ->
        files |> List.iter @@ fun source ->
          let body = Filename.remove_extension (Filename.basename source) in
          let basename = prefix ^ body ^ suffix in
          let target = Filename.concat (Filename.dirname source) basename in
          extract_doc_file snippets ~source ~target
    | [], _ -> assert false
  with
  | Failure msg ->
      prerr_endline msg;
      exit 1
  | Lexer.Syntax_error (range, msg) ->
      Printf.fprintf stderr "%a: %s\n" output_range range msg;
      exit 1

let files =
  let doc = "Source files" in
  Cmdliner.Arg.(
    value & pos_all non_dir_file [] & info [] ~docv:"FILE" ~doc)

let option_target =
  let doc = "Target file name" in
  Cmdliner.Arg.(
    value & opt string "-" & info ["o"] ~docv:"TARGET" ~doc)

let option_before =
  let doc = "Code snippet that should be inserted before the extracted code" in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["before"] ~docv:"TEXT" ~doc)

let option_before_file =
  let doc = "File that should be inserted before the extracted code" in
  Cmdliner.Arg.(
    value & opt (some non_dir_file) None &
    info ["before-file"] ~docv:"FILE" ~doc)

let option_after =
  let doc = "Code snippet that should be inserted after the extracted code" in
  Cmdliner.Arg.(
    value & opt (some string) None & info ["after"] ~docv:"TEXT" ~doc)

let option_after_file =
  let doc = "File that should be inserted after the extracted code" in
  Cmdliner.Arg.(
    value & opt (some non_dir_file) None &
    info ["after-file"] ~docv:"FILE" ~doc)

let options = Cmdliner.Term.(
    const main $ option_target $ option_before $ option_before_file $
      option_after $ option_after_file $ files)

let info =
  let doc = "Extract code from doc comments" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "ocamlcodoc" ~doc ~exits:Cmdliner.Term.default_exits ~man

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (options, info))
