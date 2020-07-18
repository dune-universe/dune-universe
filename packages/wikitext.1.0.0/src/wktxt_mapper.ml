(**
   {!mapper} allows to implement AST rewriting using open recursion.
   A typical mapper would be based on {!default_mapper}, a deep
   identity mapper, and will fall back on it for handling the syntax it
   does not modify.
*)

open Wktxt_type

type mapper =
  { document : mapper -> document -> document
  ; block  : mapper -> block -> block
  ; table_block : mapper -> table_block -> table_block
  ; inline : mapper -> inline -> inline
  }

(**/**)

let document self = List.map (self.block self)

and block self = function
  | Header (importance, content) ->
      Header (importance, (List.map (self.inline self) content))
  | Paragraph (content) ->
      Paragraph (List.map (self.inline self) content)
  | List (content_list) ->
      List (List.map (fun l -> List.map (self.block self) l) content_list)
  | NumList (content_list) ->
      NumList (List.map (fun l -> List.map (self.block self) l) content_list)
  | DefList (content_list) ->
      DefList (List.map
        (fun (l1, l2) ->
          (List.map (self.inline self) l1, List.map (self.block self) l2))
        content_list)
  | Table (title, content_list) ->
      Table ((List.map (self.inline self) title)
             , (List.map (fun l -> List.map (self.table_block self) l) content_list))
  | Hrule -> Hrule

and table_block self = function
  | TableHead content -> TableHead (List.map (self.inline self) content)
  | TableItem content -> TableItem (List.map (self.inline self) content)

and inline self = function
  | Italic l -> Italic (List.map (self.inline self) l)
  | Bold l -> Bold (List.map (self.inline self) l)
  | String _ | Link _ as x -> x

(**/**)

(** A default mapper, which implements a "deep identity" mapping. *)
let default_mapper =
  { document
  ; block
  ; table_block
  ; inline
  }

let rec noformat inlines =
  let aux = function
    | Bold x -> noformat x
    | Italic x -> noformat x
    | String _ | Link _ as x -> [ x ]
  in
  List.flatten (List.map aux inlines)

(** [toc doc]
    Compute the table of contents of [doc]. This table of contents
    is computed by looking at headers. First level header is omitted.
    Table of contents is returned as un ordered list of links pointing
    to title's anchors.
*)
let toc
  : document -> (document * block) option =
  fun doc ->
  let toc_list = ref [] in
  let cnt = ref 0 in
  let id () = incr cnt ; "wikitext-header-anchor-" ^ string_of_int !cnt in
  let block self blck =
    match blck with
    | Header (d, inlines) when d <> 1 ->
      let id = id () in
      let link = (String ("<a href=\"#" ^ id ^ "\">") :: inlines) @ [String "</a>"] in
      toc_list := ((Ordered, d - 1), [link]) :: !toc_list ;
      Header (d, String ("<span id=\"" ^ id ^ "\"></span>") :: inlines)
    | _ -> default_mapper.block self blck
  in
  let mapper = { default_mapper with block } in
  let doc = mapper.document mapper doc in
  match Wktxt_parsing_functions.parse_list 0 (List.rev !toc_list) Ordered with
  | [] -> None
  | toc :: _ -> Some (doc, toc)

(** [set_toc doc]
    Replace ["__TOC__"] in [doc] by the auto-generated table of contents.
*)
let set_toc doc =
  match toc doc with
  | None -> doc
  | Some (doc, toc) ->
    let block self blck =
      match blck with
      | Paragraph [ String "__TOC__" ] -> toc
      | List _ | NumList _ | DefList _ -> blck
      | _ -> default_mapper.block self blck
    in
    let mapper = { default_mapper with block } in
    mapper.document mapper doc

(** [link sep str]
    A very basic link creation. No escaping is performed.
    Turn [str] into a link (["<a href=\"%s\">%s</a>"]).
    If [str] contains a [sep] character, everything coming before is
    used as the url, and the rest as text.
*)
let link : char -> string -> string =
  fun sep str ->
  let link url txt = Printf.sprintf "<a href=\"%s\">%s</a>" url txt in
  match String.index_opt str sep with
  | None -> link str str
  | Some space_pos ->
    link
      (String.sub str 0 space_pos)
      (String.sub str (space_pos + 1) (String.length str - space_pos - 1))

(**
   [set_links doc]
   Replace [Link] and [ExtLink] occurences by their HTML representation.
   using {!val:link}.
   [Link] uses ['|'] as separator and [ExtLink] uses [' '].
*)
let set_links doc =
  let inline self inl =
    match inl with
    | Link (1, s) -> String (link ' ' s)
    | Link (2, s) -> String (link '|' s)
    | _ -> default_mapper.inline self inl
  in
  let mapper = { default_mapper with inline } in
  mapper.document mapper doc

(** [normalize doc]
    Concatenates following [Strings] elements together
    and removes block's leading and trailing spaces. *)
let normalize doc =
  let rec concat = function
    | [] -> []
    | Bold inl :: tl -> Bold (concat inl) :: concat tl
    | Italic inl :: tl -> Italic (concat inl) :: concat tl
    | String s1 :: String s2 :: tl -> concat (String (s1 ^ s2) :: tl)
    | hd :: tl -> hd :: concat tl
  in
  let trim_right str =
    let start = String.length str - 1 in
    let rec loop position =
      if position < 0 then ""
      else match String.get str position with
        | '\n' | ' ' | '\t' -> loop (position - 1)
        | _ ->
          if position = start then str
          else String.sub str 0 (position + 1)
    in
    loop start
  in
  let trim_left str =
    let len = String.length str in
    let start = 0 in
    let rec loop position =
      if position < len then match String.get str position with
        | '\n' | ' ' | '\t' -> loop (position + 1)
        | _ ->
          if position = start then str
          else String.sub str position (len - position)
      else ""
    in loop 0
  in
  let rec trim fst = function
    | [] ->
      []
    | [ String s ] ->
      if fst then [ String (s |> trim_left |> trim_right) ]
      else [ String (trim_right s) ]
    | String s as hd :: tl ->
      if fst then String (s |> trim_left) :: trim false tl
      else hd :: trim false tl
    | hd :: tl ->
      hd :: trim false tl
  in
  let norm contents = trim true (concat contents) in
  let block self = function
    | Header (lvl, content) ->
      Header (lvl, norm content)
    | Paragraph p ->
      Paragraph (norm p)
    | DefList l ->
      DefList (List.map (fun (t, d) -> (norm t, List.map (self.block self) d)) l)
    | Table (title, tbl) ->
      Table (norm title, (List.map (List.map (self.table_block self)) tbl))
    | b ->
      default_mapper.block self b
  and table_block _ = function
    | TableHead content -> TableHead (norm content)
    | TableItem content -> TableItem (norm content)
  and inline self = function
    | Italic l -> Italic (norm l)
    | Bold l -> Bold (norm l)
    | i -> default_mapper.inline self i
  in
  let mapper = { default_mapper with block ; table_block ; inline} in
  mapper.document mapper doc
