(* Color styles *)
let style = function
  | "constant" :: "character" :: _ -> [ANSITerminal.magenta]
  | "constant" :: "language" :: "boolean" :: _-> [ANSITerminal.cyan]
  | "comment" :: _ -> [ANSITerminal.magenta]
  | "constant" :: "numeric" :: _-> [ANSITerminal.blue]
  | "entity" :: "name" :: "tag" :: "label" :: _-> [ANSITerminal.cyan]
  | "entity" :: "name" :: "type" :: "variant" :: _-> [ANSITerminal.Bold]
  | "entity" :: "name" :: "tag" :: _ -> [ANSITerminal.green]
  | "invalid" :: _ -> [ANSITerminal.red]
  | "keyword" :: "control" :: _-> [ANSITerminal.magenta]
  | "keyword" :: "operator" :: _ -> [ANSITerminal.yellow]
  | "keyword" :: _ -> [ANSITerminal.Bold]
  | "support" :: "other" :: "module" :: _-> [ANSITerminal.green]
  | "meta" :: "module-reference" :: _ -> [ANSITerminal.green]
  | "punctuation" :: "definition" :: "comment" :: _ -> [ANSITerminal.cyan]
  | "punctuation" :: "definition" :: "string" :: _-> [ANSITerminal.magenta]
  | "string" :: _-> [ANSITerminal.magenta]
  | _ -> [ANSITerminal.Reset]

let print_block =
  List.iter
    (List.iter (fun (scope, str) ->
         ANSITerminal.print_string (style scope) str))

let rec highlight_tokens i spans line = function
  | [] -> List.rev spans
  | tok :: toks ->
    let j = TmLanguage.ending tok in
    assert (j > i);
    let text = String.sub line i (j - i) in
    let scope = match TmLanguage.scopes tok with
      | [] -> []
      | scope :: _ -> String.split_on_char '.' scope
    in highlight_tokens j ((scope, text) :: spans) line toks

let read t grammar stack =
  let rec loop stack lines =
    match read_line () with
    | exception End_of_file -> List.rev lines
    | line ->
      (* Some patterns don't work if there isn't a newline *)
      let line = line ^ "\n" in
      let tokens, stack = TmLanguage.tokenize_exn t grammar stack line in
      let spans = highlight_tokens 0 [] line tokens in
      loop stack (spans :: lines)
  in loop stack []

let () =
  if Array.length Sys.argv < 3 then
    prerr_endline
      "Read and highlight code from stdin.\
       \n\n\
       Usage: <exename> <language-name> <plist-grammar-files>..."
  else
    let source = Sys.argv.(1) in
    let t = TmLanguage.create () in
    for i = 2 to Array.length Sys.argv - 1 do
      let chan = open_in Sys.argv.(i) in
      let plist =
        Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
          ~finally:(fun () -> close_in chan)
      in
      let grammar = TmLanguage.of_plist_exn plist in
      TmLanguage.add_grammar t grammar
    done;
    match TmLanguage.find_by_name t source with
    | None -> prerr_endline ("Unknown language " ^ source)
    | Some grammar -> read t grammar TmLanguage.empty |> print_block
