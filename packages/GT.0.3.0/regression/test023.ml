@type 'a a = [`A of 'a | `B of GT.string] with show, eq, compare
@type b = [`C of GT.int | `D of GT.string] with show, eq, compare
@type 'a c = ['a a | b] with show, eq, compare

let _ =
  let x = `A 3 in
  let y = `D "2" in
  Printf.printf "%s\n" (GT.show(a) (GT.show GT.int)  x);
  Printf.printf "%s\n" (GT.show(b)                          y);
  Printf.printf "%s\n" (GT.show(c) (GT.show GT.int) x);
  Printf.printf "%s\n" (GT.show(c) (GT.show GT.int) y);
  Printf.printf "%b\n" (GT.eq(a)   (=) x x);
  Printf.printf "%b\n" (GT.eq(b)       y y);
  Printf.printf "%b\n" (GT.eq(c)   (=) x x);
  Printf.printf "%b\n" (GT.eq(c)   (=) y y);
  Printf.printf "%b\n" (GT.eq(c)   (=) x y)
