let (<--) e f = List.(concat @@ map f e)

let () =
  assert ([1;2] = [1;2]);
  assert ([%comp x || x <-- [1;2]] = [1;2]);
  assert ([%comp x || x <-- [1;2]; x mod 2 = 0] = [2]);
  (* looks bizarre but working *)
  assert ([%comp x || true || true; x <-- [1;2]] = [1;2])


    
