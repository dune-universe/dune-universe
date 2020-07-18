open Wktxt_type

let rec output_document out doc :(unit)= List.iter (output_block out) doc

and display_item : 'a. ((string -> unit) -> 'a -> unit) -> string -> (string -> unit) -> 'a list -> unit =
  fun displayer tag_name out content ->
    begin
    out ("<" ^ tag_name ^ ">") ;
    List.iter (displayer out) content ;
    out ("</" ^ tag_name ^ ">")
    end

and output_inline out inl :(unit)=
  match inl with
  | Bold (content) ->
    display_item output_inline "b" out content
  | Italic (content) ->
    display_item output_inline "i" out content
  | String str | Link (_, str) -> out str

and output_block out blck :(unit)=
  match blck with
  | Header (lvl, content) ->
    display_item output_inline ("h" ^ string_of_int lvl) out content
  | Hrule -> out "<hr>"
  | Paragraph (content) ->
    display_item output_inline "p" out content
  | List (content_list) ->
    display_item (display_item output_block "li") "ul" out content_list
  | NumList (content_list) ->
    display_item (display_item output_block "li") "ol" out content_list
  | DefList (content_list) ->
    let output_def_list_item out item = match item with
      | ([],[]) -> ()
      | (term, []) -> display_item output_inline "dt" out term
      | ([], desc) -> display_item output_block "dd" out desc
      | (term, desc) ->
        display_item output_inline "dt" out term ;
        display_item output_block "dd" out desc
    in
    display_item output_def_list_item "dl" out content_list
  | Table (title, content_list) ->
    out "<table>" ;
    if title <> [] then display_item output_inline "caption" out title ;
    let output_table_block out table_block = match table_block with
      | TableHead content -> display_item output_inline "th" out content
      | TableItem content -> display_item output_inline "td" out content
    in
    display_item (display_item output_table_block "tr") "tbody" out content_list ;
    out "</table>"
