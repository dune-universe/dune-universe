type capture = {
    capture_name : string option;
  }

module IntMap = Map.Make(Int)

type match_ = {
    name : string option;
    pattern : Pcre.regexp;
    captures : capture IntMap.t;
  }

type delim_kind = End | While

type delim = {
    delim_begin : Pcre.regexp;
    delim_end : Pcre.regexp;
    delim_patterns : pattern list;
    delim_name : string option;
    delim_content_name : string option;
    delim_begin_captures : capture IntMap.t;
    delim_end_captures : capture IntMap.t;
    delim_apply_end_pattern_last : bool;
    delim_kind : delim_kind;
  }

and pattern =
  | Match of match_
  | Delim of delim
  | Include_local of string
  | Include_scope of string
  | Include_self
  | Include_base

type repo_item_kind =
  | Repo_rule of pattern
  | Repo_patterns of pattern list

type repo_item = {
    repo_item_kind : repo_item_kind;
    repo_inner : (string, repo_item) Hashtbl.t;
  }

type grammar = {
    name : string;
    scope_name : string;
    patterns : pattern list;
    repository : (string, repo_item) Hashtbl.t;
  }

type t = {
    by_name : (string, grammar) Hashtbl.t;
    by_scope_name : (string, grammar) Hashtbl.t;
  }

type plist =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of plist list
  | `Dict of (string * plist) list
  ]

exception Error of string

let create () = {
    by_name = Hashtbl.create 23;
    by_scope_name = Hashtbl.create 23;
  }

let add_grammar t grammar =
  Hashtbl.add t.by_name (String.lowercase_ascii grammar.name) grammar;
  Hashtbl.add t.by_scope_name grammar.scope_name grammar

let find_by_name t name =
  Hashtbl.find_opt t.by_name (String.lowercase_ascii name)

let find_by_scope_name t = Hashtbl.find_opt t.by_scope_name

let error msg = raise (Error msg)

let rec find key = function
  | [] -> None
  | (k, v) :: obj ->
     if k = key then
       Some v
     else
       find key obj

let find_exn key obj =
  match find key obj with
  | Some v -> v
  | None -> error (key ^ " not found.")

let get_dict = function
  | `Dict d -> d
  | _ -> error "Type error: Expected dict."

let get_string = function
  | `String s -> s
  | _ -> error "Type error: Expected string."

let get_list f = function
  | `Array l -> List.map f l
  | _ -> error "Type error: Expected list."

let iflags = Pcre.cflags [`ANCHORED; `DOLLAR_ENDONLY; `UTF8]

let of_plist_exn plist =
  let compile_regex s =
    try Pcre.regexp ~iflags s with
    | Pcre.Error(Pcre.BadPattern(msg, pos)) ->
       error (
           "Malformed regex " ^ s ^ ": "
           ^ msg ^ " at pos " ^ Int.to_string pos ^ "."
         )
  in
  let rec get_captures acc = function
    | [] -> acc
    | (k, v) :: kvs ->
       let idx = match int_of_string_opt k with
         | Some int -> int
         | None -> error (k ^ " is not an integer.")
       in
       let v = get_dict v in
       let capture_name = match find "name" v with
         | None -> None
         | Some name -> Some (get_string name)
       in get_captures (IntMap.add idx { capture_name } acc) kvs
  in
  let rec get_patterns obj =
    find_exn "patterns" obj
    |> get_list (fun x -> get_dict x |> patterns_of_plist)
  and patterns_of_plist obj =
    match find "include" obj with
    | Some s ->
       begin match get_string s with
       | "$base" -> Include_base
       | "$self" -> Include_self
       | s ->
          let len = String.length s in
          if len > 0 && s.[0] = '#' then
            Include_local (String.sub s 1 (len - 1))
          else
            Include_scope s
       end
    | None ->
       match find "match" obj, find "begin" obj with
       | Some s, None ->
          Match {
              pattern = compile_regex (get_string s);
              name = Option.map get_string (find "name" obj);
              captures =
                match find "captures" obj with
                | None -> IntMap.empty
                | Some value -> get_captures IntMap.empty (get_dict value)
            }
       | None, Some b ->
          let e, key, delim_kind = match find "end" obj, find "while" obj with
            | Some e, None -> e, "endCaptures", End
            | None, Some e -> e, "whileCaptures", While
            | _, _ -> error "Begin patterns must either have an end or while."
          in
          let delim_begin_captures, delim_end_captures =
            match find "captures" obj with
            | Some value ->
               let captures = get_captures IntMap.empty (get_dict value) in
               captures, captures
            | None ->
               ( (match find "beginCaptures" obj with
                  | Some value -> get_captures IntMap.empty (get_dict value)
                  | None -> IntMap.empty)
               , (match find key obj with
                  | Some value -> get_captures IntMap.empty (get_dict value)
                  | None -> IntMap.empty) )
          in
          Delim {
              delim_begin = compile_regex (get_string b);
              delim_end = compile_regex (get_string e);
              delim_patterns =
                begin match find "patterns" obj with
                | None -> []
                | Some v ->
                   get_list (fun x -> get_dict x |> patterns_of_plist) v
                end;
              delim_name = Option.map get_string (find "name" obj);
              delim_content_name =
                Option.map get_string (find "contentName" obj);
              delim_begin_captures;
              delim_end_captures;
              delim_apply_end_pattern_last =
                begin match find "applyEndPatternLast" obj with
                | Some (`Int 1) -> true
                | _ -> false
                end;
              delim_kind;
            }
       | _, _ -> error "Pattern must be match, begin/end, or begin/while."
  in
  let rec get_repo_item obj =
    { repo_item_kind =
        begin match find "match" obj, find "begin" obj with
        | None, None -> Repo_patterns (get_patterns obj)
        | _, _ -> Repo_rule (patterns_of_plist obj)
        end;
      repo_inner =
        begin match find "repository" obj with
        | None -> Hashtbl.create 0
        | Some obj -> get_repo obj
        end }
  and get_repo obj =
    let hashtbl = Hashtbl.create 31 in
    List.iter (fun (k, v) ->
        let v = get_dict v in
        let item = get_repo_item v in
        Hashtbl.add hashtbl k item
      ) (get_dict obj);
    hashtbl
  in
  let obj = get_dict plist in
  { name = get_string (find_exn "name" obj)
  ; scope_name = get_string (find_exn "scopeName" obj)
  ; patterns = get_patterns obj
  ; repository =
      match find "repository" obj with
      | None -> Hashtbl.create 0
      | Some obj -> get_repo obj
  }

type token = {
    ending : int;
    scopes : string list;
  }

let ending token = token.ending

let scopes token = token.scopes

type stack_elem = {
    stack_delim : delim;
    stack_grammar : grammar;
    stack_repos : (string, repo_item) Hashtbl.t list;
    stack_scopes : string list;
  }

type stack = stack_elem list

let empty = []

let rec add_scopes scopes = function
  | [] -> scopes
  | None :: xs -> add_scopes scopes xs
  | Some x :: xs -> add_scopes (x :: scopes) xs

(* If the stack is empty, returns the main patterns associated with the
   grammar. Otherwise, returns the patterns associated with the delimiter at
   the top of the stack. *)
let next_pats grammar = function
  | [] -> grammar.patterns
  | s :: _ -> s.stack_delim.delim_patterns

let handle_captures scopes default mat_start mat_end line captures tokens =
  let rec new_scopes acc = function
    | [] -> acc
    | (_, scope) :: xs -> new_scopes (scope :: acc) xs
  in
  let _, stack, tokens =
    (* Regex captures are ordered by their left parentheses. Do a depth-first
       preorder traversal by keeping a stack of captures. *)
    IntMap.fold (fun idx capture (start, stack, tokens) ->
        (* If the capture mentions a lookahead, it can go past the bounds of its
           parent. The match is capped at the boundary for the parent. Is this
           the right decision to make? Clearly the writer of the grammar
           intended for the capture to exceed the parent in this case. *)
        match stack with
        | [] ->
           (try
              let cap_start, cap_end = Pcre.get_substring_ofs line idx in
              let cap_start = if cap_start < start then start else cap_start in
              let cap_end = if cap_end > mat_end then mat_end else cap_end in
              ( cap_start
              , [(cap_end, capture.capture_name)]
              , { scopes = add_scopes scopes [default]; ending = cap_start }
                :: tokens )
            with Not_found | Invalid_argument _ -> (start, [], tokens))
        | (top_end, top_name) :: stack' ->
           try
             let cap_start, cap_end = Pcre.get_substring_ofs line idx in
             let cap_start = if cap_start < start then start else cap_start in
             if cap_start >= top_end then
               let under = match stack' with
                 | [] -> mat_end
                 | (end_, _) :: _ -> end_
               in
               let cap_end = if cap_end > under then under else cap_end in
               ( top_end
               , (cap_end, capture.capture_name) :: stack'
               , { scopes = add_scopes scopes (new_scopes [top_name] stack')
                 ; ending = cap_start } :: tokens )
             else
               let cap_end = if cap_end > top_end then top_end else cap_end in
               ( cap_start
               , (cap_end, capture.capture_name) :: stack
               , { scopes = add_scopes scopes (new_scopes [top_name] stack)
                 ; ending = cap_start } :: tokens )
           with Not_found | Invalid_argument _ -> (start, stack, tokens)
      ) captures (mat_start, [], tokens)
  in
  let rec pop tokens = function
    | [] -> tokens
    | (ending, scope) :: stack ->
       pop
         ({ scopes = add_scopes scopes (new_scopes [scope] stack)
          ; ending } :: tokens)
         stack
  in pop tokens stack

let rec find_nested scope = function
  | [] -> None
  | repo :: repos ->
     match Hashtbl.find_opt repo scope with
     | Some x -> Some x
     | None -> find_nested scope repos

let remove_empties =
  let rec go acc = function
    | [] -> acc
    | tok :: toks ->
       let prev = match toks with
         | [] -> 0
         | tok :: _ -> tok.ending
       in
       if tok.ending = prev then
         go acc toks
       else
         go (tok :: acc) toks
  in go []

(* Tokenizes a line according to the grammar.

   [t]: The collection of grammars.
   [grammar]: The language grammar.
   [stack]: The stack that keeps track of nested delimiters
   [pos]: The current index into the string.
   [toks]: The list of tokens, with the rightmost ones at the front.
   [line]: The string that is being matched and tokenized.
   [rem_pats]: The remaining patterns yet to be tried *)
let rec match_line ~t ~grammar ~stack ~pos ~toks ~line rem_pats =
  let len = String.length line in
  let scopes, stk_pats, repos, cur_grammar = match stack with
    | [] ->
       [grammar.scope_name], grammar.patterns, [grammar.repository], grammar
    | se :: _ ->
       let d = se.stack_delim in
       ( se.stack_scopes
       , d.delim_patterns
       , se.stack_repos
       , se.stack_grammar )
  in
  (* Try each pattern in the list until one matches. If none match, increment
     [pos] and try all the patterns again. *)
  let rec try_pats repos cur_grammar ~k = function
    | [] -> k () (* No patterns have matched, so call the continuation *)
    | Match m :: pats ->
       begin match Pcre.exec ~pos ~rex:m.pattern line with
       | exception Not_found -> try_pats repos cur_grammar ~k pats
       | subs ->
          let start, end_ = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let toks = { scopes; ending = pos } :: toks in
          let toks =
            handle_captures scopes m.name pos end_ subs m.captures toks
          in
          let toks =
            { scopes = add_scopes scopes [m.name]; ending = end_ } :: toks
          in
          match_line ~t ~grammar ~stack ~pos:end_ ~toks ~line
            (next_pats grammar stack)
       end
    | Delim d :: pats ->
       (* Try to match the delimiter's begin pattern *)
       begin match Pcre.exec ~pos ~rex:d.delim_begin line with
       | exception Not_found -> try_pats repos cur_grammar ~k pats
       | subs ->
          let start, end_ = Pcre.get_substring_ofs subs 0 in
          assert (start = pos);
          let toks = { scopes; ending = pos } :: toks in
          let toks =
            handle_captures scopes d.delim_name pos end_ subs
              d.delim_begin_captures toks
          in
          let toks =
            { scopes = add_scopes scopes [d.delim_name]
            ; ending = end_ } :: toks
          in
          let se =
            { stack_delim = d
            ; stack_repos = repos
            ; stack_grammar = cur_grammar
            ; stack_scopes =
                add_scopes scopes [d.delim_name; d.delim_content_name] }
          in
          match d.delim_kind with
          | End ->
             (* Push the delimiter on the stack and continue *)
             match_line ~t ~grammar ~stack:(se :: stack) ~pos:end_ ~toks ~line
               d.delim_patterns
          | While ->
             (* Subsume the remainder of the line into a span *)
             ( remove_empties
                 ({ scopes =
                      add_scopes scopes [d.delim_name; d.delim_content_name]
                  ; ending = len } :: toks)
             , se :: stack )
       end
    | Include_scope name :: pats ->
       begin match find_by_scope_name t name with
       | None ->
          (* Grammar not found; try the next pattern. *)
          try_pats repos cur_grammar ~k pats
       | Some nested_grammar ->
          let k () = try_pats repos cur_grammar ~k pats in
          try_pats [nested_grammar.repository] nested_grammar
            nested_grammar.patterns ~k
       end
    | Include_base :: pats ->
       let k () = try_pats repos cur_grammar ~k pats in
       try_pats [grammar.repository] grammar grammar.patterns ~k
    | Include_self :: pats ->
       let k () = try_pats repos cur_grammar ~k pats in
       try_pats [cur_grammar.repository] cur_grammar cur_grammar.patterns ~k
    | Include_local key :: pats ->
       match find_nested key repos with
       | None -> error ("Unknown repository key " ^ key ^ ".")
       | Some item ->
          match item.repo_item_kind with
          | Repo_rule rule ->
             try_pats (item.repo_inner :: repos) cur_grammar (rule :: pats) ~k
          | Repo_patterns pats' ->
             let k () = try_pats repos cur_grammar ~k pats in
             try_pats (item.repo_inner :: repos) cur_grammar pats' ~k
  in
  let try_delim delim stack' ~k =
    (* Try to match the delimiter's end pattern *)
    let end_match =
      match Pcre.exec ~pos ~rex:delim.delim_end line with
      | exception Not_found -> None
      | subs ->
         let start, end_ = Pcre.get_substring_ofs subs 0 in
         assert (start = pos);
         let toks =
           { scopes =
               add_scopes scopes [delim.delim_name; delim.delim_content_name]
           ; ending = pos } :: toks in
         let toks =
           handle_captures scopes delim.delim_name pos end_ subs
             delim.delim_end_captures toks
         in Some (end_, toks)
    in
    match delim.delim_kind, end_match with
    | End, None -> k ()
    | End, Some (end_, toks) ->
       let toks =
         { scopes = add_scopes scopes [delim.delim_name]
         ; ending = end_ } :: toks
       in
       (* Pop the delimiter off the stack and continue *)
       match_line ~t ~grammar ~stack:stack' ~pos:end_ ~toks ~line
         (next_pats grammar stack')
    | While, Some (_, toks) ->
       (* Subsume the remainder of the line into a span *)
       ( remove_empties
           ({ scopes = add_scopes scopes [delim.delim_name]; ending = len }
            :: toks)
       , stack )
    | While, None -> k ()
  in
  if pos > len then
    (* End of string reached *)
    match stack with
    | [] -> (remove_empties ({ scopes; ending = len } :: toks), stack)
    | se :: stack' ->
       let d = se.stack_delim in
       match d.delim_kind with
       | End ->
          ( remove_empties
              ({ scopes = add_scopes scopes [d.delim_name]
               ; ending = len } :: toks)
          , stack )
       (* If reached, this means that the while pattern wasn't matched. Retry
          the line. *)
       | While ->
          match_line ~t ~grammar ~stack ~pos:0 ~toks:[] ~line
            (next_pats grammar stack')
  else
    (* No patterns have matched, so increment the position and try again *)
    let k () =
      match_line ~t ~grammar:cur_grammar ~stack ~pos:(pos + 1) ~toks ~line
        stk_pats
    in
    match stack with
    | [] -> try_pats repos grammar rem_pats ~k
    | se :: stack' ->
       if se.stack_delim.delim_apply_end_pattern_last then
         try_pats repos se.stack_grammar rem_pats
           ~k:(fun () -> try_delim se.stack_delim stack' ~k)
       else
         try_delim se.stack_delim stack'
           ~k:(fun () -> try_pats repos se.stack_grammar rem_pats ~k)

let tokenize_exn t grammar stack line =
  match_line ~t ~grammar ~stack ~pos:0 ~toks:[] ~line (next_pats grammar stack)
