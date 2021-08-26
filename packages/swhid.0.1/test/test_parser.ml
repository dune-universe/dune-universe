let () =
  let ok_test_cases =
    [| "swh:1:cnt:28d0af969b32e69a389087d7a267a2ecc05f1350"
     ; "swh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6b"
     ; "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505"
     ; "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
     ; "swh:1:rel:22ece559cc7cc2364edc5e5593d63ae8bd229f9f"
     ; "swh:1:snp:c7c108084bc0bf3d81436bf980b46e98bd338453"
     ; "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=9-15"
     ; "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=15"
    |]
  in
  Array.iter
    (fun input ->
      match Swhid.Parse.from_string input with
      | Error _s -> assert false
      | Ok res ->
        let result = Format.asprintf "%a" Swhid.Pp.identifier res in
        let ok = input = result in
        if not ok then
          Format.eprintf
            "error: expected `%s` when parsing and printing `%s` but got `%s`@."
            input input result;
        assert ok )
    ok_test_cases;

  let error_test_cases =
    [| (*
        * Lexing error
        *)
       ( "swh:1:cnt:28d0af969b32e69389087d7a267a2ecc05f1350&"
       , "lexer error: Unexpected char: &" )
     ; ("\n", "lexer error: Unexpected char: \n")
       (*
        * Parsing errors
        *)
       (* invalid prefix "sh" should be swh *)
     ; ( "sh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6b"
       , "parser error: Scheme incorrect" )
       (* invalid object id "L33T" is too short *)
     ; ("swh:1:dir:L33T", "parser error: Invalid object id") (* *)
     ; ( "swh:1:cnt:ffacc412c4e5fz55cafccd6e58bc58072c17ff6b"
       , "parser error: Invalid object id" )
       (* *)
     ; ( "swh:2:cnt:ffacc412c4e5fa55cafccd6e58bc58072c17ff6b"
       , "parser error: Invalid scheme versions" )
       (* invalid object id "ffacc412c4e5ff55cafccd6e58bc58072c17ff:" has an unwanted ':' *)
     ; ( "swh:1:cnt:ffacc412c4e5ff55cafccd6e58bc58072c17ff6:"
       , "parser error: Invalid object id" )
       (* invalid object type "yeet" should be "rev" *)
     ; ( "swh:1:yeet:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
       , "parser error: Object_type invalid" )
       (* fragment qualifier non-compliant "thecakeisalie" should be "lines" *)
     ; ( "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;thecakeisalie=9-15"
       , "parser error: Fragment_qualifier non-compliant" )
       (* context qualifier absolute path wrong "pafff" should be "path"*)
     ; ( "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;pafff=/Examples/SimpleFarm/simplefarm.ml;lines=9-15"
       , "parser error: Context_qualifier absolute path wrong" )
       (* context qualifier origin url "orangejuice" should be "origin" *)
     ; ( "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;orangejuice=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=9-15"
       , "parser error: Context_qualifier origin url" )
       (* context qualifier visit/anchor incorrect "encore" should be "anchor" *)
     ; ( "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;encore=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=9-15"
       , "parser error: Context_qualifier visit/anchor incorrect" )
       (* context qualifier visit/anchor incorrect "visir" should be "visit" *)
     ; ( "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visir=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=9-15"
       , "parser error: Context_qualifier visit/anchor incorrect" )
       (*
        * Syntax errors
        *)
       (* Syntax error, negative line *)
     ; ( "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;anchor=swh:1:rev:2db189928c94d62a3b4757b3eec68f0a4d4113f0;path=/Examples/SimpleFarm/simplefarm.ml;lines=-15"
       , "parser error: syntax error" )
     ; ("", "parser error: syntax error")
     ; (":", "parser error: syntax error")
     ; (";", "parser error: syntax error")
     ; ("=", "parser error: syntax error")
     ; ("-", "parser error: syntax error")
     ; ("cC666/._", "parser error: syntax error")
     ; ("666", "parser error: syntax error")
     ; ( "https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git"
       , "parser error: syntax error" )
    |]
  in
  Array.iter
    (fun (input, err_msg) ->
      match Swhid.Parse.from_string input with
      | Ok _res -> assert false
      | Error error ->
        let ok = error = err_msg in
        if not ok then
          Format.eprintf
            "error:@.expected `%s`@.when parsing and printing@.`%s`@.but got \
             `%s`@."
            err_msg input error;
        assert ok )
    error_test_cases
