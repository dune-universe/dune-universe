type exists_node = Node : 'a Soup.node -> exists_node

type scope = string list

type scope_stack = scope list

type selector = {
  select : scope_stack;
  excludes : scope_stack list;
}

type token = {
  background : string option;
  foreground : string option;
  selectors : selector list;
}

type theme = {
  tokens : token list;
}

let find_exn key obj =
  match List.assoc_opt key obj with
  | Some v -> v
  | None -> failwith (key ^ " not found.")

let get_dict = function
  | `Dict d -> d
  | _ -> failwith "Type error: Expected dict."

let get_list f = function
  | `Array l -> List.map f l
  | _ -> failwith "Type error: Expected list."

let get_string = function
  | `String s -> s
  | _ -> failwith "Type error: Expected string."

let token_of_plist (plist : Plist_xml.t) : token option =
  (* TODO: Handle selector substraction operator *)
  let make select = { select; excludes = [] } in
  let d = get_dict plist in
  match List.assoc_opt "scope" d with
  | None -> None
  | Some scope ->
    let selectors =
      scope
      |> get_string
      |> String.split_on_char ','
      |> List.map (fun str ->
          str
          |> String.split_on_char ' '
          |> List.map String.trim
          |> List.filter ((<>) "")
          |> List.rev
          |> List.map (String.split_on_char '.')
          |> make)
    in
    let settings = find_exn "settings" d |> get_dict in
    Some
      { background =
          Option.map get_string (List.assoc_opt "background" settings)
      ; foreground =
          Option.map get_string (List.assoc_opt "foreground" settings)
      ; selectors }

let theme_of_plist plist =
  let d = get_dict plist in
  let tokens = find_exn "settings" d in
  let tokens = get_list Fun.id tokens in
  { tokens = List.filter_map token_of_plist tokens }

let prefix_length scope selector =
  let rec loop acc scope selector =
    match scope, selector with
    | [], _ :: _ -> None (* Selector is more specific than the scope *)
    | [], [] -> Some acc
    | x :: xs, y :: ys when x = y -> loop (acc + 1) xs ys
    | _ :: _, _ -> Some acc
  in loop 0 scope selector

let rec score_selector scopes_stack (sels : scope_stack) =
  (* TextMate's scoring system is arcane and the documentation does not give
     a complete specification:

     - https://macromates.com/manual/en/scope_selectors
     - http://textmate.1073791.n5.nabble.com/formal-definition-of-scope-selector-syntax-td12109.html
     - https://macromates.com/blog/2005/introduction-to-scopes/

     This specification just adds up all the depths. *)
  match scopes_stack, sels with
  | [], _ :: _ -> None
  | scopes :: scopes_stack, sel :: sels ->
    Option.bind (prefix_length scopes sel) begin fun len ->
      Option.map ((+) len) (score_selector scopes_stack sels)
    end
  | _, [] -> Some 0

let score_token scopes_stack (token : token) =
  let f acc next =
    match acc, score_selector scopes_stack next.select with
    | None, None -> None
    | None, Some score -> Some score
    | Some _, None -> acc
    | Some score1, Some score2 ->
      if score1 > score2 then
        Some score1
      else
        Some score2
  in List.fold_left f None token.selectors

let style_of_token token =
  let color =
    match token.foreground with
    | None -> ""
    | Some foreground -> "color: " ^ foreground ^ ";"
  in
  color

let create_node theme scopes i j line =
  assert (j > i);
  let inner_text = String.sub line i (j - i) in
  let scopes = List.map (String.split_on_char '.') scopes in
  let token =
    List.fold_left (fun acc next ->
        let new_score = score_token scopes next in
        match acc with
        | None ->
          begin match new_score with
            | None -> None
            | Some _ -> Some next
          end
        | Some old ->
          let old_score = score_token scopes old in
          match old_score, new_score with
          | None, None -> None
          | None, Some _ -> Some next
          | Some _, None -> Some old
          | Some score1, Some score2 ->
            if score1 < score2 then
              Some next
            else
              Some old
      ) None theme.tokens
  in
  match token with
  | Some token ->
    Node
      (Soup.create_element
         "span"
         ~attributes:["style", style_of_token token]
         ~inner_text)
  | None -> Node (Soup.create_text inner_text)

let rec highlight_tokens theme i acc line = function
  | [] -> List.rev acc
  | tok :: toks ->
    let j = TmLanguage.ending tok in
    let span = create_node theme (TmLanguage.scopes tok) i j line in
    highlight_tokens theme j (span :: acc) line toks

let highlight_line langs grammar theme stack line =
  let tokens, stack = TmLanguage.tokenize_exn langs grammar stack line in
  let nodes = highlight_tokens theme 0 [] line tokens in
  let a = Soup.create_element "a" ~class_:"sourceLine" in
  List.iter (fun (Node node) -> Soup.append_child a node) nodes;
  a, stack

(* Maps over the list while keeping track of some state.
   Discards the state at the end because I don't need it. *)
let rec map_fold f acc = function
  | [] -> []
  | x :: xs ->
    let y, acc = f acc x in
    y :: map_fold f acc xs

(* Splits a string into lines, keeping the newline at the end. Assumes that
   the string ends with a newline. *)
let lines s =
  let rec loop lines i =
    match String.index_from_opt s i '\n' with
    | None -> List.rev lines
    | Some j -> loop (String.sub s i (j - i + 1) :: lines) (j + 1)
  in
  loop [] 0

let highlight_block langs grammar theme code =
  let lines = lines code in
  let a's =
    try
      map_fold (highlight_line langs grammar theme) TmLanguage.empty lines
    with
    | Oniguruma.Error s -> failwith s
    | TmLanguage.Error s -> failwith s
  in
  let code = Soup.create_element "code" ~class_:"highlight" in
  List.iter (Soup.append_child code) a's;
  let pre = Soup.create_element "pre" in
  Soup.append_child pre code;
  pre
