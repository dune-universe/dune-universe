let re syn str =
  match
    Oniguruma.create
      str Oniguruma.Options.none Oniguruma.Encoding.ascii
      syn
  with
  | Error e ->
    prerr_endline e;
    assert false
  | Ok r -> r

let test_syn_match syn regex str =
  match Oniguruma.match_ (re syn regex) str 0 Oniguruma.Options.none with
  | None -> assert false
  | Some _ -> ()

let () =
  test_syn_match Oniguruma.Syntax.asis "()" "()";
  test_syn_match Oniguruma.Syntax.asis "[a-b]" "[a-b]"
