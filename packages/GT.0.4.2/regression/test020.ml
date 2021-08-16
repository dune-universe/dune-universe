@type 'a t = A of GT.int | B of GT.string | C of 'a GT.list with eq, compare

let _ =
  let x   = A 1 in
  let y   = B "2" in
  let z   = C [4] in
  let z'  = C [4] in
  let z'' = C [] in
  let compare x y =
    let open GT in
    match transform(t) (new @t[compare]
                         (fun y -> transform(int) (new @int[compare]) y)) x y with
    | GT.GT -> "GT"
    | GT.LT -> "LT"
    | GT.EQ -> "EQ"
  in
  Printf.printf "x   == x: %s\n" (compare x   x);
  Printf.printf "x   == y: %s\n" (compare x   y);
  Printf.printf "y   == x: %s\n" (compare y   x);
  Printf.printf "x   == z: %s\n" (compare x   z);
  Printf.printf "z'  == z: %s\n" (compare z'  z);
  Printf.printf "z'' == z: %s\n" (compare z'' z)
