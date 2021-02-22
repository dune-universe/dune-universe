let rec print_type indent ty history =
  let type_kind = Clang.get_type_kind ty in
  let sub_indent = indent ^ "  " in
  if type_kind <> Invalid then
    begin
      let type_kind_str =
        Refl.show [%refl: Clang.clang_ext_typekind] []
          (Clang.ext_type_get_kind ty) in
      Printf.printf "%s- \"%s\": %s\n" sub_indent
        (String.escaped (Clang.get_type_spelling ty))
        type_kind_str;
      begin
        match type_kind with
        | Pointer ->
            print_type sub_indent (Clang.get_pointee_type ty) history
        | _ -> ()
      end;
      let decl = Clang.get_type_declaration ty in
      if not (Clang.is_invalid (Clang.get_cursor_kind decl)) &&
        not (List.exists (Clang.equal_cursors decl) history) then
        print_tree sub_indent decl history;
      match Clang.ext_attributed_type_get_attr_kind ty with
      | NoAttr -> ()
      | attr ->
          Printf.printf "%s+ %s" sub_indent
            (Clang.ext_attr_kind_get_spelling attr)
    end;

and print_type_loc indent ty history =
  let cl = Clang.ext_type_loc_get_class ty in
  Printf.printf "%s- %s (%s)\n" indent
    (Refl.show [%refl: Clang.clang_ext_typeloc_class] [] cl)
    (Clang.get_type_spelling (Clang. ext_type_loc_get_type ty));
  let sub_indent = indent ^ "  " in
  begin
    match cl with
    | Pointer ->
        print_type_loc sub_indent (Clang.ext_pointer_like_type_loc_get_pointee_loc ty) history
    | _ -> ()
  end

and print_tree indent cursor history =
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
    | kind ->
        Printf.sprintf "%s (%s)"
          (Clang.get_cursor_kind_spelling kind)
          (Refl.show [%refl: Clang.cxcursorkind] [] kind) in
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
  print_type sub_indent ty history;
  begin
    match kind with
    | VarDecl ->
        print_type_loc indent (Clang.ext_declarator_decl_get_type_loc cursor)
          history
    | _ -> ()
  end;
  assert (Clang.visit_children cursor @@ fun cur _par ->
    print_tree sub_indent cur history;
    Continue)

let print ast tu =
  Clang.format_diagnostics Clang.warning_or_error Format.err_formatter tu;
  if ast then
    Format.printf "@[%a@]@." Clang.Translation_unit.pp
      (Clang.Ast.of_cxtranslationunit tu)
  else
    print_tree "" (Clang.get_translation_unit_cursor tu) []

let main ast exprs files =
  Clangml_tools_common.command_line begin fun language command_line_args ->
    let options = Clang.default_editing_translation_unit_options () in
  (*
    let options =
      Clang.Cxtranslationunit_flags.(
        Clang.default_editing_translation_unit_options ()
        + include_attributed_types) in
  *)
    exprs |> List.iter begin fun expr ->
      let suffix = Clang.suffix_of_language language in
      let filename = "string" ^ suffix in
      prerr_endline filename;
      let tu = Clang.parse_string ~command_line_args ~filename expr ~options in
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
