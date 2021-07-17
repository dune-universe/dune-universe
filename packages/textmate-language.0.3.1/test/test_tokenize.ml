open Util

type test_line = {
  line : string;
  expected : (int * string list) list
}

let check_find grammar scope_name filetypes () =
  let open TmLanguage in
  let t = create () in
  let check p =
    Alcotest.check
      Alcotest.bool scope_name true
      (p (TmLanguage.find_by_scope_name t scope_name));
    List.iter (fun filetype ->
        Alcotest.check
          Alcotest.bool filetype true
          (p (TmLanguage.find_by_filetype t filetype));
      ) filetypes
  in
  check ((=) None);
  add_grammar t grammar;
  check begin function
    | None -> false
    | Some grammar' -> grammar == grammar'
  end

let test_find filename scope_name filetypes =
  ( filename
  , [ Alcotest.test_case
        "Yojson"
        `Quick
        (check_find (read_yojson_basic filename) scope_name filetypes)
    ; Alcotest.test_case
        "Ezjsonm"
        `Quick
        (check_find (read_ezjsonm filename) scope_name filetypes) ] )

let check_tokenize grammar name cases () =
  let open TmLanguage in
  let t = create () in
  add_grammar t grammar;
  let tested_type = Alcotest.(list (pair int (list string))) in
  let check lines =
    ignore (List.fold_left (fun stack { line; expected } ->
        let toks, stack =
          tokenize_exn t (Option.get (find_by_scope_name t name)) stack line
        in
        let toks = List.map (fun tok -> (ending tok, scopes tok)) toks in
        Alcotest.check tested_type line expected toks;
        stack) empty lines)
  in
  List.iter check cases

let test_tokenize filename scope_name cases =
  ( filename
  , [ Alcotest.test_case
        "Yojson"
        `Quick
        (check_tokenize (read_yojson_basic filename) scope_name cases)
    ; Alcotest.test_case
        "Ezjsonm"
        `Quick
        (check_tokenize (read_ezjsonm filename) scope_name cases) ] )

let () =
  Alcotest.run "Finding" [
    test_find "data/multiwhile.json" "source.multiwhile" ["mw"; "multiwhile"]
  ];
  Alcotest.run "Highlighting" [
    test_tokenize "data/a.json" "source.a" [
      [
        { line = "a"
        ; expected = [ 1, ["keyword.letter"; "source.a"] ] }
      ];
      [
        { line = "a(a)"
        ; expected =
            [ 1, ["keyword.letter"; "source.a"]
            ; 2, ["punctuation.paren.open"; "source.a"]
            ; 3, ["keyword.letter"; "expression.group"; "source.a"]
            ; 4, ["punctuation.paren.close"; "source.a"] ] }
      ];
      [
        { line = "a("
        ; expected =
            [ 1, ["keyword.letter"; "source.a"]
            ; 2, ["punctuation.paren.open"; "source.a"] ] };
        { line = "a)"
        ; expected =
            [ 1, ["keyword.letter"; "expression.group"; "source.a" ]
            ; 2, ["punctuation.paren.close"; "source.a"] ] }
      ]
    ];
    test_tokenize "data/while.json" "source.while" [
      [
        { line = "a"
        ; expected = [ 1, ["begin"; "source.while"] ] }
      ];
      [
        { line = "ac"
        ; expected =
            [ 1, ["begin"; "source.while"]
            ; 2, ["expression.group"; "source.while"] ] };
        { line = "bc"
        ; expected =
            [ 1, ["while"; "source.while"]
            ; 2, ["keyword.letter"; "expression.group"; "source.while"] ] }
      ]
    ];
    (* See https://github.com/microsoft/vscode-textmate/issues/25 *)
    test_tokenize "data/multiwhile.json" "source.multiwhile" [
      [
        { line = "X"
        ; expected = [ 1, ["xbegin"; "source.multiwhile"] ] };
        { line = "xY"
        ; expected =
            [ 1, ["xwhile"; "source.multiwhile"]
            ; 2, ["ybegin"; "xlist"; "source.multiwhile"] ] };
        { line = "yxy"
        ; expected =
            [ 1, ["source.multiwhile"]
            ; 2, ["xwhile"; "source.multiwhile"]
            ; 3, ["ywhile"; "xlist"; "source.multiwhile"] ] };
        { line = "xy"
        ; expected =
            [ 1, ["xwhile"; "source.multiwhile"]
            ; 2, ["ywhile"; "xlist"; "source.multiwhile"] ] };
        { line = "y"
        ; expected = [ 1, ["source.multiwhile"] ] }
      ];
    ]
  ]
