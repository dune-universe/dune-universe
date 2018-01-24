let () =
  match 3, 2 with
  | (x, y) when x = y -> assert false
  | (x, y) when w <-- x + y; w = 4 -> assert false
  | (x, y) when Some w <-- Some (x + y); w = 4 -> assert false
  | (x, y) when Some w <-- None; w = x + y -> assert false
  | (x, y) when x = 3; y = 2; Some w <-- Some (x + y); w = 5 -> prerr_endline "ok"
  | _ -> assert false
