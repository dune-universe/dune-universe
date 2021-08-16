@type a = A of b | C of GT.int GT.list with show
and   b = B of a | D of GT.string      with show

let x = A (B (C [1; 2; 3; 4]))
let y = B (A (D "3"))

let () = Printf.printf "%s\n" @@ GT.show(a) x

class show_a2stub prereq =
  object
    inherit [_] show_a_t_stub prereq as super
    method! c_C () a ys = "new " ^ super#c_C () a ys
  end

let show_a_new eta = let (f, _) = fix_a (new show_a2stub) show_b_0 in f eta

let a = { a with plugins = object
                   method show eta = show_a_new () eta
                 end}
let _ =
  Printf.printf "%s\n" (GT.show(b) y);
  Printf.printf "%s\n" (GT.show(a) x);

