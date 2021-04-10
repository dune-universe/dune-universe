open Common

type token = {
  ending : int;
  scopes : string list;
}

let ending token = token.ending

let scopes token = token.scopes

type stack_elem = {
  stack_delim : delim;
  stack_region : Oniguruma.Region.t;
  stack_begin_line : string;
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

(* Should the character be escaped in a regex? *)
let is_special ch =
  ch = '\\' ||
  ch = '\'' ||
  ch = '|' ||
  ch = '.' ||
  ch = '*' ||
  ch = '+' ||
  ch = '?' ||
  ch = '^' ||
  ch = '$' ||
  ch = '-' ||
  ch = ':' ||
  ch = '~' ||
  ch = '#' ||
  ch = '&' ||
  ch = '(' || ch = ')' ||
  ch = '[' || ch = ']' ||
  ch = '{' || ch = '}' ||
  ch = '<' || ch = '>'

(* Insert the substring of [line] from [beg] to [end_] into [buf]. *)
let insert_capture buf line beg end_ =
  let rec loop i =
    if i = end_ then
      ()
    else
      let ch = line.[i] in
      if is_special ch then
        Buffer.add_char buf '\\';
      Buffer.add_char buf ch;
      loop (i + 1)
  in
  loop beg

(* Substitute the begin pattern's captures for the backreferences in the end
   delimiter. *)
let subst_backrefs stack_top =
  let { stack_delim = { delim_end = regex_str; delim_begin = begin_re; _ }
      ; stack_begin_line = line
      ; stack_region = region
      ; _ } = stack_top in
  let buf = Buffer.create (String.length regex_str) in
  let num_beg_captures = Oniguruma.num_captures begin_re in
  let rec loop i escaped =
    if i < String.length regex_str then
      match regex_str.[i], escaped with
      | '\\', true ->
        Buffer.add_string buf "\\\\";
        loop (i + 1) false
      | '\\', false ->
        loop (i + 1) true
      | char, true ->
        if char >= '0' && char <= '9' then (
          let idx = Char.code char - Char.code '0' in
          if idx < num_beg_captures then
            let beg = Oniguruma.Region.capture_beg region idx in
            let end_ = Oniguruma.Region.capture_end region idx in
            if beg <> -1 then
              insert_capture buf line beg end_
        ) else (
          Buffer.add_char buf '\\';
          Buffer.add_char buf char
        );
        loop (i + 1) false
      | char, false ->
        Buffer.add_char buf char;
        loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents buf

let rec find_nested scope = function
  | [] -> None
  | repo :: repos ->
    match Hashtbl.find_opt repo scope with
    | Some x -> Some x
    | None -> find_nested scope repos

(* Discard zero-length tokens. *)
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

(* Emit tokens for the match region's captures. *)
let handle_captures
    scopes default mat_start mat_end region captures tokens =
  let rec new_scopes acc = function
    | [] -> acc
    | (_, scope) :: xs -> new_scopes (scope :: acc) xs
  in
  let _, stack, tokens =
    (* Regex captures are ordered by their left parentheses. Do a depth-first
       preorder traversal by keeping a stack of captures. *)
    IntMap.fold (fun idx capture (start, stack, tokens) ->
        (* If the capture mentions a lookahead, it can go past the bounds of
           its parent. The match is capped at the boundary for the parent. Is
           this the right decision to make? Clearly the writer of the grammar
           intended for the capture to exceed the parent in this case. *)
        if idx < 0 || idx >= Oniguruma.Region.length region then
          (start, stack, tokens)
        else
          let cap_start = Oniguruma.Region.capture_beg region idx in
          let cap_end = Oniguruma.Region.capture_end region idx in
          if cap_start = -1 then
            (* Capture wasn't found, ignore *)
            (start, stack, tokens)
          else
            match stack with
            | [] ->
               let cap_start = if cap_start < start then start else cap_start in
               let cap_end = if cap_end > mat_end then mat_end else cap_end in
               let tokens =
                 { scopes = add_scopes scopes [default]
                 ; ending = cap_start }
                 :: tokens
               in
               ( cap_start
               , [(cap_end, capture.capture_name)]
               , tokens )
            | (top_end, top_name) :: stack' ->
               let cap_start = if cap_start < start then start else cap_start in
               if cap_start >= top_end then
                 let under = match stack' with
                   | [] -> mat_end
                   | (end_, _) :: _ -> end_
                 in
                 let cap_end = if cap_end > under then under else cap_end in
                 let tokens =
                   { scopes = add_scopes scopes (new_scopes [top_name] stack')
                   ; ending = cap_start } :: tokens
                 in
                 (* Pop off the stack, then push the capture *)
                 ( top_end
                 , (cap_end, capture.capture_name) :: stack'
                 , tokens )
               else
                 let cap_end = if cap_end > top_end then top_end else cap_end in
                 let tokens =
                   { scopes = add_scopes scopes (new_scopes [top_name] stack)
                   ; ending = cap_start } :: tokens
                 in
                 (* Push the capture on the stack *)
                 ( cap_start
                 , (cap_end, capture.capture_name) :: stack
                 , tokens )
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
      ([grammar.scope_name], grammar.patterns, [grammar.repository], grammar)
    | se :: _ ->
      let d = se.stack_delim in
      (se.stack_scopes, d.delim_patterns, se.stack_repos, se.stack_grammar)
  in
  (* Try each pattern in the list until one matches. If none match, increment
     [pos] and try all the patterns again. *)
  let rec try_pats repos cur_grammar ~k = function
    | [] -> k () (* No patterns have matched, so call the continuation *)
    | Match m :: pats ->
      let match_result =
        Oniguruma.match_ m.pattern line pos Oniguruma.Options.none
      in
      begin match match_result with
        | None -> try_pats repos cur_grammar ~k pats
        | Some region ->
          let start = Oniguruma.Region.capture_beg region 0 in
          let end_ = Oniguruma.Region.capture_end region 0 in
          assert (start = pos);
          let toks = { scopes; ending = pos } :: toks in
          let toks =
            handle_captures scopes m.name pos end_ region m.captures toks
          in
          let toks =
            { scopes = add_scopes scopes [m.name]; ending = end_ } :: toks
          in
          match_line ~t ~grammar ~stack ~pos:end_ ~toks ~line
            (next_pats grammar stack)
      end
    | Delim d :: pats ->
      (* Try to match the delimiter's begin pattern *)
      let match_result =
        Oniguruma.match_ d.delim_begin line pos Oniguruma.Options.none
      in
      begin match match_result with
        | None -> try_pats repos cur_grammar ~k pats
        | Some region ->
          let start = Oniguruma.Region.capture_beg region 0 in
          let end_ = Oniguruma.Region.capture_end region 0 in
          assert (start = pos);
          let toks = { scopes; ending = pos } :: toks in
          let toks =
            handle_captures scopes d.delim_name pos end_ region
              d.delim_begin_captures toks
          in
          let toks =
            { scopes = add_scopes scopes [d.delim_name]
            ; ending = end_ } :: toks
          in
          let se =
            { stack_delim = d
            ; stack_region = region
            ; stack_begin_line = line
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
  let try_delim stack_top stack' ~k =
    (* Try to match the delimiter's end pattern *)
    let delim = stack_top.stack_delim in
    let end_match =
      match
        Oniguruma.create (subst_backrefs stack_top)
          Oniguruma.Options.none Oniguruma.Encoding.utf8
          Oniguruma.Syntax.default
      with
      | Error e -> error ("End pattern: " ^ delim.delim_end ^ ": " ^ e)
      | Ok re ->
        match Oniguruma.match_ re line pos Oniguruma.Options.none with
        | None -> None
        | Some region ->
          let start = Oniguruma.Region.capture_beg region 0 in
          let end_ = Oniguruma.Region.capture_end region 0 in
          assert (start = pos);
          let toks =
            { scopes =
                add_scopes scopes [delim.delim_name; delim.delim_content_name]
            ; ending = pos } :: toks in
          let toks =
            handle_captures scopes delim.delim_name pos end_ region
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
          ~k:(fun () -> try_delim se stack' ~k)
      else
        try_delim se stack'
          ~k:(fun () -> try_pats repos se.stack_grammar rem_pats ~k)

let tokenize_exn t grammar stack line =
  match_line ~t ~grammar ~stack ~pos:0 ~toks:[] ~line (next_pats grammar stack)
