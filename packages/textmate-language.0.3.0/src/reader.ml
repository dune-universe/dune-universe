open Common

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
  | `Assoc d | `Dict d | `O d -> d
  | _ -> error "Type error: Expected dict."

let get_string = function
  | `String s -> s
  | _ -> error "Type error: Expected string."

let get_list f = function
  | `A l | `Array l | `List l -> List.map f l
  | _ -> error "Type error: Expected list."

let compile_regex re =
  match
    Oniguruma.create
      re Oniguruma.Options.none
      Oniguruma.Encoding.utf8 Oniguruma.Syntax.default
  with
  | Error msg -> error (re ^ ": " ^ msg)
  | Ok re -> re

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
    in
    let capture_patterns = match find "patterns" v with
      | None -> []
      | Some v -> get_pattern_list v
    in
    get_captures (IntMap.add idx { capture_name; capture_patterns } acc) kvs
and get_pattern_list l = get_list (fun x -> patterns_of_plist (get_dict x)) l
and get_patterns obj = find_exn "patterns" obj |> get_pattern_list
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
      Match
        { pattern = compile_regex (get_string s)
        ; name = Option.map get_string (find "name" obj)
        ; captures =
            match find "captures" obj with
            | None -> IntMap.empty
            | Some value -> get_captures IntMap.empty (get_dict value) }
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
      Delim
        { delim_begin = compile_regex (get_string b)
        ; delim_end = get_string e
        ; delim_patterns =
            begin match find "patterns" obj with
              | None -> []
              | Some v -> get_pattern_list v
            end
        ; delim_name = Option.map get_string (find "name" obj)
        ; delim_content_name = Option.map get_string (find "contentName" obj)
        ; delim_begin_captures
        ; delim_end_captures
        ; delim_apply_end_pattern_last =
            begin match find "applyEndPatternLast" obj with
              | Some (`Int 1) -> true
              | _ -> false
            end
        ; delim_kind }
    | _, _ -> error "Pattern must be match, begin/end, or begin/while."

let of_doc_exn (plist : union) =
  let rec get_repo_item obj =
    { repo_item_kind =
        begin match find "match" obj, find "begin" obj with
          | None, None -> Repo_patterns (get_patterns obj)
          | _, _ -> Repo_rule (patterns_of_plist obj)
        end
    ; repo_inner =
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
      | Some obj -> get_repo obj }

let of_plist_exn = (of_doc_exn :> plist -> grammar)

let of_ezjsonm_exn = (of_doc_exn :> ezjsonm -> grammar)

let of_yojson_exn = (of_doc_exn :> yojson -> grammar)
