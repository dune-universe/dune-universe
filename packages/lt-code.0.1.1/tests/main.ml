let () =
  let alco_suites = [] in
  let qc_suites =
    [ ("Encode_decode.Qc", Encode_decode.Qc.suite) ]
    |> List.map (fun (name, test) ->
           (name, List.map QCheck_alcotest.to_alcotest test))
  in
  let suites = alco_suites @ qc_suites in
  Alcotest.run "ofountain" suites
