@type a = A of b | C of GT.int GT.list with show
and   b = B of c | D of GT.string with show
and   c = E of a with show

class show_a_new ((_,fb,_) as prereq) =
  object
    inherit [_] @a[show] prereq as super
    method! c_C () x y = "new " ^ super#c_C () x y
    method! c_A () _ x = "new A " ^ (fb () x)
  end

let show_a_new eta =
  let (f,_,_) = fix_a (new show_a_new) (new show_b_t_stub) (new show_c_t_stub) in
  f eta


let _ =
  let x = A (B (E (C [1; 2; 3; 4]))) in
  let y = B (E (A (D "3"))) in
  Printf.printf "%s\n" (GT.transform(a) (new @a[show]) () x);
  Printf.printf "%s\n" (GT.transform(b) (new @b[show]) () y);
  Printf.printf "%s\n" (show_a_new () x);
