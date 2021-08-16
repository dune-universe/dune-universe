@type 'a t = A of 'a GT.option GT.list with show, eq

let _ =
  let x = A [Some 1; None; Some 2; None]   in
  let y = A [Some 1; None; Some 2; Some 4] in
  let z = A [] in
  let si = string_of_int in
  Printf.printf "x=%s\n" (GT.show(t) si x);
  Printf.printf "y=%s\n" (GT.show(t) si y);
  Printf.printf "z=%s\n" (GT.show(t) si z);
  Printf.printf "x == x = %b\n" (GT.eq(t) (=) x x);
  Printf.printf "x == y = %b\n" (GT.eq(t) (=) x y);
  Printf.printf "x == z = %b\n" (GT.eq(t) (=) x z)
