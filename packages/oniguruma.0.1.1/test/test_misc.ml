let re str =
  match
    Oniguruma.create
      str Oniguruma.Options.none Oniguruma.Encoding.ascii
      Oniguruma.Syntax.default
  with
  | Error e ->
    prerr_endline e;
    assert false
  | Ok r -> r

let () =
  prerr_endline Oniguruma.version;
  assert (Oniguruma.num_captures (re "a") = 0);
  assert (Oniguruma.num_captures (re "(a)") = 1);
  assert (Oniguruma.num_captures (re "(a)()") = 2);
  assert (Oniguruma.num_captures (re "(a)(b(c))") = 3)
