let test_cases =
  [ ( Swhid.Lang.content ~hash_type:"sha1_git"
        "7bdf38d4468c114206c9b6ebd9cf1176e085d346" []
    , Ok
        [ "https://archive.softwareheritage.org/api/1/content/sha1_git:7bdf38d4468c114206c9b6ebd9cf1176e085d346/raw/"
        ] )
  ; ( Swhid.Lang.release "208f61cc7a5dbc9879ae6e5c2f95891e270f09ef" []
    , Ok
        [ "https://archive.softwareheritage.org/api/1/vault/directory/4453cfbdab1a996658cd1a815711664ee7742380/raw/"
        ] )
  ; ( Swhid.Lang.snapshot "6a3a2cf0b2b90ce7ae1cf0a221ed68035b686f5a" []
    , Ok
        [ "https://archive.softwareheritage.org/api/1/vault/directory/eb4f88b555061f611d4d7182a0a36e5e771a73ad/raw/"
        ; "https://archive.softwareheritage.org/api/1/vault/directory/b53b0637f5ced64109d78c7bc32ea3bccbb6106c/raw/"
        ; "https://archive.softwareheritage.org/api/1/vault/directory/fcbc6c0ea6d27a85d6fcb3cbf9d9168e3dafd096/raw/"
        ; "https://archive.softwareheritage.org/api/1/vault/directory/3eb35765545dcca55cb5a7f30ab31d794cc36c95/raw/"
        ] )
  ]

let pp_result fmt = function
  | Ok l ->
    Format.fprintf fmt "OK: %a@."
      (fun fmt -> List.iter (fun s -> Format.fprintf fmt "%s, " s))
      l
  | Error e ->
    Format.fprintf fmt "ERROR: %a@."
      (fun fmt -> List.iter (fun s -> Format.fprintf fmt "%s, " s))
      e

let () =
  (* TODO: we disable tests on windows because SSL certs are wrong on windows CI, see https://github.com/ocaml/setup-ocaml/issues/205 *)
  if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then
    ()
  else
    List.iter
      (fun (identifier, expected_result) ->
        let result = Swhid.Download.any identifier in
        let ok = result = expected_result in
        if not ok then begin
          Format.eprintf "test failed for identifier %a@." Swhid.Pp.identifier
            identifier;
          Format.eprintf "expected: %a@." pp_result expected_result;
          Format.eprintf "got: %a@." pp_result result;
          assert false
        end )
      test_cases
