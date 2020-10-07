let example = {|
struct foo {
  /** bla */
  int bla;
};
|}

let () =
  let ast = Clang.Ast.parse_string example in
  let bla =
    match ast with
    | { desc = { items = [
        { desc = RecordDecl { fields = [bla]; _ }; _ }]; _ }; _ } ->
        bla
    | _ -> assert false in
  assert
    (Clang.cursor_get_raw_comment_text (Clang.Ast.cursor_of_node bla)
      = "/** bla */")

let example = {|
struct foo {
  int bla; // comment
  int bar;
  int tux; // comment2
    // ctd
};
|}

let range_start_of_node node =
  Clang.get_range_start (Clang.get_cursor_extent
    (Clang.Ast.cursor_of_node node))

let range_end_of_node node =
  Clang.get_range_end (Clang.get_cursor_extent (Clang.Ast.cursor_of_node node))

let comment_of_range (tu : Clang.cxtranslationunit)
    (range : Clang.cxsourcerange) : string list =
  let rec aux accu tokens =
    match tokens with
    | [] -> accu
    | hd :: tl ->
        match Clang.get_token_kind hd with
        | Punctuation -> aux accu tl
        | Comment -> aux (Clang.get_token_spelling tu hd :: accu) tl
        | _ -> accu in
  List.rev (aux [] (Array.to_list (Clang.tokenize tu range)))

let fields_with_comment (record_decl : Clang.Decl.t)
    : (Clang.Decl.t * string list) list =
  let fields =
    match record_decl with
    | { desc = RecordDecl { fields; _ }; _ } -> fields
    | _ -> invalid_arg "fields_with_comment" in
  match fields with
  | [] -> []
  | hd :: tl ->
      let tu =
        Clang.cursor_get_translation_unit
          (Clang.Ast.cursor_of_node record_decl) in
      let record_decl_end = range_end_of_node record_decl in
      let rec aux accu first others =
        let first_end = range_end_of_node first in
        match others with
        | [] ->
            let comment =
              comment_of_range tu (Clang.get_range first_end record_decl_end) in
            List.rev ((first, comment) :: accu)
        | hd :: tl ->
            let hd_start = range_start_of_node hd in
            let comment =
              comment_of_range tu (Clang.get_range first_end hd_start) in
            aux ((first, comment) :: accu) hd tl in
      aux [] hd tl

let () =
  let ast = Clang.Ast.parse_string example in
  let foo =
    match ast with
    | { desc = { items = [foo]; _ }; _ } -> foo
    | _ -> assert false in
  match fields_with_comment foo with
  | [({ desc = Field { name = "bla"; _ }; _ }, ["// comment"]);
      ({ desc = Field { name = "bar"; _ }; _ }, []);
      ({ desc = Field { name = "tux"; _ }; _ }, ["// comment2"; "// ctd"])] ->
        ()
  | _ -> assert false

let example = {|
struct foo {
  // bla
  int bla; // comment
};
|}

let () =
  let clang_options =
    Clang.Cxtranslationunit_flags.(
      Clang.default_editing_translation_unit_options () +
      detailed_preprocessing_record) in
  let ast = Clang.Ast.parse_string ~clang_options example in
  Format.printf "@[%a@]@." (Refl.pp [%refl: Clang.Ast.translation_unit] []) ast;
  let bla =
    match List.rev ast.desc.items with
    | { desc = RecordDecl { fields = [bla]; _ }; _ } :: _ ->
        bla
    | _ -> assert false in
  print_endline
    (Clang.cursor_get_raw_comment_text (Clang.Ast.cursor_of_node bla))
