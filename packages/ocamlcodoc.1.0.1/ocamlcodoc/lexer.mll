{
  type range = {
      start_pos : Lexing.position;
      end_pos : Lexing.position;
    }

  exception Syntax_error of range * string

  type delimiter_kind =
    | Open_codoc | Comment | String | String_ident of string | Square_bracket

  type open_delimiter = {
      position : Lexing.position;
      kind : delimiter_kind;
      mutable warned : bool;
    }

  type 'a context = {
      out_channel : 'a;
      delimiter_stack : open_delimiter Stack.t;
      warnings : (range * string) Queue.t;
      mutable important_warnings : bool;
    }

  let is_in_string context =
    match Stack.top context.delimiter_stack with
    | { kind = (String | String_ident _) } ->
        true
    | _ ->
        false

  let try_close_delimiter context delimiter_kind =
    try
      context.delimiter_stack |> Stack.iter begin fun { kind } ->
        if kind = delimiter_kind then
          raise Exit
      end;
      false
    with Exit ->
      let rec pop_loop only_open_codoc =
        match (Stack.pop context.delimiter_stack).kind with
        | Open_codoc ->
            pop_loop only_open_codoc
        | kind when kind = delimiter_kind ->
            only_open_codoc
        | _ ->
            pop_loop false in
      pop_loop true

  let mismatched_delimiters context delimiter end_pos =
    context.important_warnings <- true;
    if not delimiter.warned then
      begin
        delimiter.warned <- true;
        let range = { start_pos = delimiter.position; end_pos } in
        Queue.push (range, "Mismatched delimiters") context.warnings;
      end
}

let ident = ['a' - 'z' '_'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_']*

rule main context = parse
| "(**" {
  doc_comment lexbuf.lex_curr_p context lexbuf
}
| "(*" | "(***" {
  code_comment lexbuf.lex_curr_p { context with out_channel = None } lexbuf;
  main context lexbuf
}
| "(*{[" {
  let position = lexbuf.lex_start_p in
  Utils.output_position context.out_channel position;
  Stack.push { position; kind = Open_codoc; warned = false }
    context.delimiter_stack;
  codoc position context lexbuf
}
| "\"" {
  string lexbuf.lex_curr_p { context with out_channel = None } lexbuf;
  main context lexbuf;
}
| "{" (ident? as delim) "|" {
  ident_string lexbuf.lex_curr_p { context with out_channel = None } delim
    lexbuf;
  main context lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  main context lexbuf
}
| eof {
  ()
}
| _ {
  main context lexbuf
}
and doc_comment start_pos context = parse
| "*)" {
  main context lexbuf
}
| "{[" {
  let position = lexbuf.lex_start_p in
  Utils.output_position context.out_channel position;
  Stack.push { position; kind = Open_codoc; warned = false }
    context.delimiter_stack;
  codoc start_pos context lexbuf
}
| "(*" {
  code_comment lexbuf.lex_curr_p { context with out_channel = None } lexbuf;
  doc_comment start_pos context lexbuf
}
| "\"" {
  string lexbuf.lex_curr_p { context with out_channel = None } lexbuf;
  doc_comment start_pos context lexbuf
}
| "{" (ident? as delim) "|" {
  ident_string lexbuf.lex_curr_p { context with out_channel = None } delim
    lexbuf;
  main context lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  doc_comment start_pos context lexbuf
}
| eof {
  let range = { start_pos; end_pos = lexbuf.lex_curr_p} in
  raise (Syntax_error (range, "Unterminated doc-comment"))
}
| _ {
  doc_comment start_pos context lexbuf
}
and codoc start_pos context = parse
| "]}" {
  begin
    match Stack.top context.delimiter_stack with
    | { kind = Open_codoc } ->
        ignore (Stack.pop context.delimiter_stack);
    | { warned = false } as delimiter ->
        delimiter.warned <- true;
        let range = {
          start_pos = delimiter.position;
          end_pos = lexbuf.lex_curr_p } in
        let warning =
          (range, "End of pre-formatted code before closing delimiter") in
        Queue.push warning context.warnings
    | { warned = true } -> ()
  end;
  doc_comment start_pos context lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  output_char context.out_channel '\n';
  codoc start_pos context lexbuf
}
| "(*" {
  if not (is_in_string context) then
    Stack.push
      { position = lexbuf.lex_start_p; kind = Comment; warned = false }
      context.delimiter_stack;
  output_string context.out_channel "(*";
  codoc start_pos context lexbuf
}
| "\"" {
  begin
    match
      try
        Some (Stack.top context.delimiter_stack)
      with Stack.Empty ->
        None
    with
    | Some { kind = String_ident _ } ->
        ()
    | Some { kind = String } ->
        ignore (Stack.pop context.delimiter_stack)
    | _ ->
        if not (try_close_delimiter context String) then
          Stack.push
            { position = lexbuf.lex_start_p; kind = String; warned = false }
            context.delimiter_stack
  end;
  output_string context.out_channel "\"";
  codoc start_pos context lexbuf
}
| ("{" (ident? as delim) "|") as start_string {
  if not (is_in_string context) then
    Stack.push
      { position = lexbuf.lex_start_p; kind = String_ident delim;
        warned = false } context.delimiter_stack;
  output_string context.out_channel start_string;
  codoc start_pos context lexbuf
}
| "[" {
  if not (is_in_string context) then
    Stack.push
      { position = lexbuf.lex_start_p; kind = Square_bracket; warned = false }
      context.delimiter_stack;
  output_string context.out_channel "[";
  codoc start_pos context lexbuf
}
| "*)" {
  begin
    match Stack.top context.delimiter_stack with
    | { kind = (String | String_ident _) } ->
        ()
    | { kind = Comment } ->
        ignore (Stack.pop context.delimiter_stack)
    | delimiter ->
        if not (try_close_delimiter context Comment) then
          mismatched_delimiters context delimiter lexbuf.lex_curr_p
  end;
  output_string context.out_channel "*)";
  codoc start_pos context lexbuf
}
| ("|" (ident? as delim) "}") as end_string {
  begin
    match Stack.top context.delimiter_stack with
    | { kind = String_ident delim' } when delim = delim' ->
        ignore (Stack.pop context.delimiter_stack)
    | { kind = (String | String_ident _) } ->
        ()
    | delimiter ->
        if not (try_close_delimiter context (String_ident delim)) then
          mismatched_delimiters context delimiter lexbuf.lex_curr_p
  end;
  output_string context.out_channel end_string;
  codoc start_pos context lexbuf
}
| "]" {
  begin
    match Stack.top context.delimiter_stack with
    | { kind = (String | String_ident _) } ->
        ()
    | { kind = Square_bracket } ->
        ignore (Stack.pop context.delimiter_stack)
    | delimiter ->
        if not (try_close_delimiter context Square_bracket) then
          mismatched_delimiters context delimiter lexbuf.lex_curr_p
  end;
  output_string context.out_channel "]";
  codoc start_pos context lexbuf
}
| eof {
  let range = { start_pos; end_pos = lexbuf.lex_curr_p} in
  raise (Syntax_error (range, "Unterminated code in doc-comment"))
}
| _ as char {
  output_char context.out_channel char;
  codoc start_pos context lexbuf
}
and code_comment start_pos context = parse
| "(*" {
  Option.iter (fun out_channel -> output_string out_channel "(*")
      context.out_channel;
  code_comment start_pos context lexbuf;
  code_comment start_pos context lexbuf
}
| "*)" {
  Option.iter (fun out_channel -> output_string out_channel "*)")
    context.out_channel;
  ()
}
| "\"" {
  Option.iter (fun out_channel -> output_string out_channel "\"")
    context.out_channel;
  ignore (string lexbuf.lex_curr_p context lexbuf);
  code_comment start_pos context lexbuf
}
| "\n" {
  Option.iter (fun out_channel -> output_string out_channel "\n")
    context.out_channel;
  Lexing.new_line lexbuf;
  code_comment start_pos context lexbuf
}
| eof {
  let range = { start_pos; end_pos = lexbuf.lex_curr_p} in
  raise (Syntax_error (range, "Unterminated code comment"))
}
| _ {
  code_comment start_pos context lexbuf
}
and string start_pos context = parse
| "\"" {
  Option.iter (fun out_channel -> output_string out_channel "\"")
    context.out_channel;
}
| ("\\" _) as s {
  Option.iter (fun out_channel -> output_string out_channel s)
     context.out_channel;
  string start_pos context lexbuf
}
| "\n" {
  Option.iter (fun out_channel -> output_string out_channel "\n")
    context.out_channel;
  Lexing.new_line lexbuf;
  string start_pos context lexbuf
}
| eof {
  let range = { start_pos; end_pos = lexbuf.lex_curr_p} in
  raise (Syntax_error (range, "Unterminated string"))
}
| _ as char {
  Option.iter (fun context -> output_char context char)
      context.out_channel;
  string start_pos context lexbuf
}
and ident_string start_pos context delim = parse
| ("|" (ident? as delim') "}") as end_string {
  Option.iter (fun out_channel -> output_string out_channel end_string)
      context.out_channel;
  if delim = delim' then
    ()
  else
    ident_string start_pos context delim lexbuf
}
| ("\\" _) as s {
  Option.iter (fun out_channel -> output_string out_channel s)
      context.out_channel;
  ident_string start_pos context delim lexbuf
}
| "\n" {
  Lexing.new_line lexbuf;
  Option.iter (fun out_channel -> output_string out_channel "\n")
    context.out_channel;
  ident_string start_pos context delim lexbuf
}
| eof {
  let range = { start_pos; end_pos = lexbuf.lex_curr_p} in
  raise (Syntax_error (range, "Unterminated string"))
}
| _ as char {
  Option.iter (fun out_channel -> output_char out_channel char)
    context.out_channel;
  ident_string start_pos context delim lexbuf
}
