@type a = [`A of b | `C of GT.int] with show
and   b = [`B of a | `D of GT.string] with show

@type c = [`E of GT.int GT.list | b] with show

class ['self] show_c' prereq = object
  inherit ['self] @c[show] prereq as super
  method c_E i x y = "new " ^ super#c_E i x y
  method c_B i x y = "new " ^ super#c_B i x y
  method c_D i x y = "new " ^ super#c_D i x y
end

let _ =
  let x = `A (`B (`C 3)) in
  let y = `B (`A (`D "3")) in
  let z = `E [1; 2; 3] in
  Printf.printf "%s\n" (GT.transform(a) (new @a[show]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @b[show]) () y);
  Printf.printf "%s\n" (GT.transform(c) (new @c[show]) () z);
  Printf.printf "%s\n" (GT.transform(c) (new @c[show]) () y);
  Printf.printf "%s\n" (GT.transform(c) (new show_c') () z);
  Printf.printf "%s\n" (GT.transform(c) (new show_c') () y)
