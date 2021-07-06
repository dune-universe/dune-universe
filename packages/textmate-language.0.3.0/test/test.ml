type test_line = {
  line : string;
  expected : (int * string list) list
}

let read_file filename =
  let chan = open_in filename in
  Fun.protect (fun () ->
      let buf = Buffer.create 256 in
      let rec loop () =
        match input_line chan with
        | exception End_of_file -> Buffer.contents buf
        | line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          loop ()
      in loop())
    ~finally:(fun () -> close_in chan)

let read_plist filename =
  let chan = open_in filename in
  let plist =
    Fun.protect (fun () -> Markup.channel chan |> Plist_xml.parse_exn)
      ~finally:(fun () -> close_in chan)
  in
  TmLanguage.of_plist_exn plist

let read_yojson_basic filename =
  TmLanguage.of_yojson_exn (Yojson.Basic.from_file filename)

let read_ezjsonm filename =
  let chan = open_in filename in
  let json =
    Fun.protect (fun () -> Ezjsonm.from_channel chan)
      ~finally:(fun () -> close_in chan)
  in
  TmLanguage.of_ezjsonm_exn json

let check data name cases () =
  let open TmLanguage in
  let t = create () in
  add_grammar t data;
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

let test filename name cases =
  ( name
  , [ Alcotest.test_case
        "Yojson" `Quick (check (read_yojson_basic filename) name cases)
    ; Alcotest.test_case
        "Ezjsonm" `Quick (check (read_ezjsonm filename) name cases) ] )

let () =
  Alcotest.run "Suite1" [
    test "data/a.json" "source.a" [
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
    test "data/while.json" "source.while" [
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
    test "data/multiwhile.json" "source.multiwhile" [
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
