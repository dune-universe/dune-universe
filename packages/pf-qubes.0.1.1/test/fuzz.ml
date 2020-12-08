let go () =
  let buf = Bytes.make 1024 '\000' in
  let rec aux () =
    match Unix.(read stdin buf 0 1024) with
    | 0 -> ()
    | _ ->
      let rule = Pf_qubes.Parse_qubes.parse_qubes ~number:0 (Bytes.to_string buf) in
      match rule with
      | Error s -> Format.eprintf "error: %s\n%!" s; aux ()
      | Ok rule -> Format.printf "%a\n%!" Pf_qubes.Parse_qubes.pp_rule rule; aux ()
  in
  aux ()

let () = AflPersistent.run go
