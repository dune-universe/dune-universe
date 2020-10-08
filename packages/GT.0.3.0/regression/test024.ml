@type 'a a = [`A of 'a | `B of GT.string] with show, eq, compare
@type 'a b = [`C of 'a | `D of GT.string] with show, eq, compare
@type ('a, 'b) c = ['a a | 'b b] with show, eq, compare

let _ =
  let x = `A 3 in
  let y = `C 2 in
  Printf.printf "%s\n" (GT.show(a) (GT.show GT.int) x);
  Printf.printf "%s\n" (GT.show(b) (GT.show GT.int) y);
  Printf.printf "%s\n" (GT.show(c) (GT.show GT.int) string_of_int x);
  Printf.printf "%s\n" (GT.show(c) (GT.show GT.int) string_of_int y);
  Printf.printf "%b\n" (GT.eq(a)  (=) x x);
  Printf.printf "%b\n" (GT.eq(b)  (=) y y);
  Printf.printf "%b\n" (GT.eq(c)  (=) (=) x x);
  Printf.printf "%b\n" (GT.eq(c)  (=) (=) y y);
  Printf.printf "%b\n" (GT.eq(c)  (=) (=) x y);
  ()
