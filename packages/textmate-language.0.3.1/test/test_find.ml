open Util

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

let () =
  Alcotest.run "Finding" [
    test_find "data/multiwhile.json" "source.multiwhile" ["mw"; "multiwhile"]
  ]
