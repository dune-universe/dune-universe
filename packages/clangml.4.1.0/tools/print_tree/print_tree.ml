let rec print_tree indent cursor history =
  let history = cursor :: history in
  let kind = Clang.get_cursor_kind cursor in
  let kind_spelling =
    match kind with
    | UnexposedExpr | UnexposedStmt ->
        Refl.show [%refl: Clang.clang_ext_stmtkind] []
          (Clang.ext_stmt_get_kind cursor)
    | UnexposedDecl | VarDecl ->
        Refl.show [%refl: Clang.clang_ext_declkind] []
          (Clang.ext_decl_get_kind cursor)
    | kind -> Clang.get_cursor_kind_spelling kind in
  let spelling =
    match kind with
    | IntegerLiteral ->
        Clang.string_of_cxint (Clang.ext_integer_literal_get_value cursor)
    | FloatingLiteral ->
        Clang.string_of_cxfloat (Clang.ext_floating_literal_get_value cursor)
    | _ ->
        "\"" ^ String.escaped (Clang.get_cursor_spelling cursor) ^ "\"" in
  Printf.printf "%s* %s: %s\n" indent spelling kind_spelling;
  let sub_indent = indent ^ "  " in
  let ty = Clang.get_cursor_type cursor in
  let type_kind = Clang.get_type_kind ty in
  if type_kind <> Invalid then
    begin
      let type_kind =
        Refl.show [%refl: Clang.clang_ext_typekind] []
          (Clang.ext_type_get_kind ty) in
      Printf.printf "%s- \"%s\": %s\n" sub_indent
        (String.escaped (Clang.get_type_spelling ty))
        type_kind;
      let decl = Clang.get_type_declaration ty in
      if not (Clang.is_invalid (Clang.get_cursor_kind decl)) &&
        not (List.exists (Clang.equal_cursors decl) history) then
        print_tree (sub_indent ^ "  ") decl history;
      match Clang.ext_type_get_attribute_kind ty with
      | NoAttr -> ()
      | attr ->
          Printf.printf "%s+ %s" sub_indent
            (Clang.ext_attr_kind_get_spelling attr)
    end;
  assert (Clang.visit_children cursor @@ fun cur _par ->
    print_tree sub_indent cur history;
    Continue)

let print ast tu =
  if ast then
    Format.printf "@[%a@]@." Clang.Translation_unit.pp
      (Clang.Ast.of_cxtranslationunit tu)
  else
    print_tree "" (Clang.get_translation_unit_cursor tu) []

let main ast exprs files =
  Clangml_tools_common.command_line begin fun command_line_args ->
    let options = Clang.default_editing_translation_unit_options () in
  (*
    let options =
      Clang.Cxtranslationunit_flags.(
        Clang.default_editing_translation_unit_options ()
        + include_attributed_types) in
  *)
    exprs |> List.iter begin fun expr ->
      let tu = Clang.parse_string ~command_line_args expr ~options in
      print ast tu
    end;
    files |> List.iter begin fun file ->
      let tu = Clang.parse_file ~command_line_args file ~options in
      print ast tu
    end
  end

let option_expr =
  let doc = "One-line expression." in
  Cmdliner.Arg.(
    value & opt_all string [] & info ["c"] ~docv:"EXPR" ~doc)

let option_ast =
  let doc = "Print ClangML AST." in
  Cmdliner.Arg.(
    value & flag & info ["ast"] ~doc)

let files =
  let doc = "File to check" in
  Cmdliner.Arg.(
    value & pos_all non_dir_file [] &
    info [] ~docv:"FILE" ~doc)

let options =
  Clangml_tools_common.options
    Cmdliner.Term.(const main $ option_ast $ option_expr $ files)

let info =
  let doc = "print Clang internal AST" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "print_tree" ~doc ~exits:Cmdliner.Term.default_exits ~man

let () =
  Cmdliner.Term.eval (options, info) |>
  Cmdliner.Term.exit
