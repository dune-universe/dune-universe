@type t = A of GT.int | B of GT.string | C of GT.int GT.list with eq

let _ =
  let x = A 1 in
  let y = B "2" in
  let z = C [4] in
  let compare x y = GT.transform(t) (new @t[eq]) x y in
  Printf.printf "x == x: %b\n" (compare x x);
  Printf.printf "x == y: %b\n" (compare x y);
  Printf.printf "x == z: %b\n" (compare x z)
