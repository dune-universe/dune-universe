(* The same as test026 but with polymorphic variants *)
@type ('a, 'b) a = [`A of 'a | `B of 'b] with show, eq
@type ('a, 'b) b = [('b, 'a) a] with show, eq

let _ =
  let x = `A 3 in
  let y = `A "2" in
  Printf.printf "%s\n" (GT.show(a) (GT.show GT.int)    (GT.show GT.string)  x);
  Printf.printf "%s\n" (GT.show(a) (GT.show GT.string) (GT.show GT.int)     y);
  Printf.printf "%s\n" (GT.show(b) (GT.show GT.string) (GT.show GT.int)     x);
  Printf.printf "%s\n" (GT.show(b) (GT.show GT.int)    (GT.show GT.string)  y);
  Printf.printf "%b\n" (GT.eq(a) (=) (=) x x);
  Printf.printf "%b\n" (GT.eq(b) (=) (=) x x);
