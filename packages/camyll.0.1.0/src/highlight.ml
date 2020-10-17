let contains_at idx str prefix =
  let pre_len = String.length prefix in
  let str_len = String.length str in
  if idx + pre_len > str_len then
    false
  else
    String.sub str idx pre_len = prefix

let begins_with = contains_at 0

type rules_tree = Rule of string * string option * rules_tree list

let rec eval_rules idx str = function
  | [] -> None
  | (Rule(name, klass, children)) :: rules ->
     if contains_at idx str name then
       match eval_rules (idx + String.length name) str children with
       | Some a -> Some a
       | None -> klass
     else
       eval_rules idx str rules

let get_class str =
  let rules =
    [ Rule("comment", Some "co", [])
    ; Rule("constant", None,
           [ Rule(".numeric", Some "bn", [])
           ; Rule(".language", Some "cn", []) ])
    ; Rule("keyword", Some "kw",
           [ Rule(".control", Some "cf", [])
           ; Rule(".operator", Some "op", []) ])
    ; Rule("entity", None,
           [ Rule(".name", Some "va",
                  [ Rule(".function", Some "fu", [])
                  ; Rule(".type", Some "dt", []) ])
           ]
        )
    ; Rule("variable", Some "va", [])
    ]
  in
  eval_rules 0 str rules

type exists_node = Node : 'a Soup.node -> exists_node

let create_node scopes i j line =
  assert (j > i);
  let inner_text = String.sub line i (j - i) in
  let class_ = match scopes with
    | [] -> None
    | scope :: _ -> get_class scope
  in
  match class_ with
  | Some class_ -> Node (Soup.create_element ~class_ "span" ~inner_text)
  | None -> Node (Soup.create_text inner_text)

let rec highlight_tokens i acc line = function
  | [] -> List.rev acc
  | tok :: toks ->
     let j = TmLanguage.ending tok in
     let span = create_node (TmLanguage.scopes tok) i j line in
     highlight_tokens j (span :: acc) line toks


(** Maps over the list while keeping track of some state.
    Discards the state because I don't need it. *)
let rec map_fold f acc = function
  | [] -> []
  | x :: xs ->
     let y, acc = f acc x in
     y :: map_fold f acc xs

let highlight_block langs grammar code =
  let lines = String.split_on_char '\n' code in
  (* Some patterns don't work if there isn't a newline *)
  let lines = List.map (fun s -> s ^ "\n") lines in
  let a's =
    map_fold (fun stack line ->
        let tokens, stack = TmLanguage.tokenize_exn langs grammar stack line in
        let nodes = highlight_tokens 0 [] line tokens in
        let a = Soup.create_element "a" ~class_:"sourceLine" in
        List.iter (fun (Node node) -> Soup.append_child a node) nodes;
        a, stack) TmLanguage.empty lines
  in
  let code = Soup.create_element "code" in
  List.iter (Soup.append_child code) a's;
  let pre = Soup.create_element "pre" in
  Soup.append_child pre code;
  pre
