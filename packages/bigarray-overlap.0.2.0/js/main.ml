let () =
  let a = Bigarray.Array0.create Bigarray.char Bigarray.c_layout in
  let v = Overlap.array0 a a in
  Format.printf "Javascript support: ok (overlap: %b)\n%!" v
