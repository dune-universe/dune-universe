(* Camlp5 version of test802 *)
open Printf

module PV : sig
  @type a = [`A of b | `C of GT.int] with show
  and   b = [`B of a | `D of GT.string] with show
end = struct
  @type a = [`A of b | `C of GT.int] with show
  and   b = [`B of a | `D of GT.string] with show
end

module Show2 = struct
  open PV
  class ['self] show_b_t_stub2 (for_a,for_b) = object
    inherit ['self] show_b_t_stub (for_a,for_b)
    method c_C () (_ :b) a  = Printf.sprintf "new C (%s)" (for_a () a)
    method c_D () _ s  = Printf.sprintf "new D %s" s
  end

  let showa0 a = Printf.printf "new!\n"; new show_a_t_stub  a
  let showb0 a = Printf.printf "new!\n"; new show_b_t_stub2 a

  let show_a () s =
    (fst @@ fix_a showa0 showb0) () s

  let show_b () s =
    (snd @@ fix_a showa0 showb0) () s

  let _ = Printf.printf "%s\n" (show_a () (`A (`B (`A (`D "4")))))
end

let () = ();;

@type c = [ PV.b | `E of GT.int (* GT.list *) ] with show

(* TODO: collect plugins for mutual type declarations *)
(*
let x = `A (`B (`C 3))
let y = `B (`A (`D "3"))
let z = `E 1

let () =
  Printf.printf "%s\n" (GT.show(PV.a)   x);
  Printf.printf "%s\n" (GT.show(PV.b)   y);
  Printf.printf "%s\n" (GT.show(c)   z);
  Printf.printf "%s\n" (GT.show(c)   y);
  *)
module ShowC = struct
  open PV

  class ['extra] show_c_stub2 make_clas =
    let show_a2,show_b2 =
      Show2.(fix_a
               showa0
               (fun _ -> ((make_clas ()) (* :> 'extra show_b_t_stub *)) ))
    in
    object
      inherit [unit, 'extra, string] c_t

      inherit [ 'extra ] show_b_t_stub (show_a2,show_b2)
      method! c_B () _ a  = sprintf "new `B (%s)" (show_a2 () a)
      method! c_D () _ s  = sprintf "new `D %s" s
      method  c_E () _ s  = sprintf "new `E %d" s
    end

  let rec showc0 () = Printf.printf "new c0!\n"; new show_c_stub2 showc0

  let show_c () (s: c) =
    let trait () s = gcata_c (showc0 ()) () (s :> c) in
    trait () s

  let _ =
    Printf.printf "%s\n" (show_c () (`B (`A (`D "4"))));
    Printf.printf "%s\n" (show_c () (`E 18) )

end
