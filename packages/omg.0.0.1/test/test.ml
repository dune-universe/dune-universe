let source = "foo bar baz qux"

let test_triples () =
  assert (
    Omg.triples ["foo"; "bar"; "baz"; "qux"]
    = [("foo", "bar", "baz"); ("bar", "baz", "qux")] )

let test_forward () =
  let wanted = ["foo bar baz qux"; "bar baz qux"; "baz qux"] in
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated = Omg.generate_markov_text generator 30 (None, None) false in
    assert (List.mem generated wanted)
  done ;
  let seen = List.map (fun el -> (el, ref false)) wanted in
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated = Omg.generate_markov_text generator 30 (None, None) false in
    List.iter (fun (el, seen) -> if el = generated then seen := true) seen
  done ;
  List.iter
    (fun (el, seen) ->
      if not !seen then failwith (Format.sprintf "didn't see #%s#" el))
    seen

let test_backward () =
  let wanted = ["foo bar baz qux"; "foo bar baz"; "foo bar"] in
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated = Omg.generate_markov_text generator 30 (None, None) true in
    assert (List.mem generated wanted)
  done ;
  let seen = List.map (fun el -> (el, ref false)) wanted in
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated = Omg.generate_markov_text generator 30 (None, None) true in
    List.iter (fun (el, seen) -> if el = generated then seen := true) seen
  done ;
  List.iter
    (fun (el, seen) ->
      if not !seen then failwith (Format.sprintf "didn't see #%s#" el))
    seen

let test_force_seed_forward () =
  let wanted = "bar baz qux" in
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated =
      Omg.generate_markov_text generator 30 (Some "bar", None) false
    in
    if not (generated = wanted) then
      failwith
        (Format.sprintf "generated = #%s# and wanted = #%s#@." generated wanted)
  done ;
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated =
      Omg.generate_markov_text generator 30 (Some "bar", Some "baz") false
    in
    if not (generated = wanted) then
      failwith
        (Format.sprintf "generated = #%s# and wanted = #%s#@." generated wanted)
  done

let test_force_seed_backward () =
  let wanted = "foo bar" in
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated =
      Omg.generate_markov_text generator 30 (Some "bar", None) true
    in
    if not (generated = wanted) then
      failwith
        (Format.sprintf "generated = #%s# and wanted = #%s#@." generated wanted)
  done ;
  for _ = 1 to 1000 do
    let generator = Omg.init () in
    Omg.feed generator source ;
    let generated =
      Omg.generate_markov_text generator 30 (Some "bar", Some "foo") true
    in
    if not (generated = wanted) then
      failwith
        (Format.sprintf "generated = #%s# and wanted = #%s#@." generated wanted)
  done

let _ =
  Format.printf "testing triples...@." ;
  test_triples () ;
  Format.printf "testing forward...@." ;
  test_forward () ;
  Format.printf "testing backward...@." ;
  test_backward () ;
  Format.printf "testing force seed forward...@." ;
  test_force_seed_forward () ;
  Format.printf "test force seed backward...@." ;
  test_force_seed_backward () ;
  Format.printf "Tests are OK !@."
