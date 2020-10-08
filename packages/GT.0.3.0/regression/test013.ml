@type a = [`A of GT.int | `B of b] with show
and   b = [`C of GT.string | `D of a] with show

@type c = [a | b] with show

(* Doesn't work for now *)
class show_c' fself =
  object
    inherit [c] @c[show] fself
    method! c_C () _ s = "new C " ^ s
  end


class [ 'fself ] show_b' prereq =
  object
    inherit [ 'fself ] show_b_t_stub prereq
    method! c_C () _ s = "new C " ^ s
  end

let show_b_new eta = let (_,f) = fix_a (new show_a_t_stub) (new show_b') in f eta

let _ =
  let y = `D (`B (`C "5")) in
  Printf.printf "%s\n" (GT.transform(c) (new @c[show]) () y);
  Printf.printf "%s\n" (show_b_new () y)
