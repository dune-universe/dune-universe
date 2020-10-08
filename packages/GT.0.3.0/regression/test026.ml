@type ('a, 'b) a = A of 'a | B of 'b with show, eq
@type ('a, 'b) b = ('b, 'a) a with show, eq

let _ =
  let x = A 3 in
  let y = A "2" in
  let string x = x in

  Printf.printf "%s\n" (GT.show(a) GT.(show int) string  x);
  Printf.printf "%s\n" (GT.show(a) string GT.(show int)  y);
  Printf.printf "%s\n" (GT.show(b) string GT.(show int)  x);
  Printf.printf "%s\n" (GT.show(b) GT.(show int) string  y);
  Printf.printf "%b\n" (GT.eq(a) (=) (=) x x);
  Printf.printf "%b\n" (GT.eq(b) (=) (=) x x);
