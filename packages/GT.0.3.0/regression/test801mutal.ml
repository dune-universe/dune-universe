(* currently disabled *)
open Printf

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of GT.int a | J | K of b
and   all = (GT.int a) GT.list
[@@deriving gt ~options:{show;gmap}]

(*
class ['self_b] show_b_hack ((show_a,_,_) as prereq) = object
  inherit ['self_b] show_b_t_stub prereq
  method c_I inh___037_ _ _x__038_ =
    Printf.sprintf "I {%s}"
      (show_a (GT.lift @@ GT.show GT.int) () _x__038_)
  method c_K () _ x = Printf.sprintf "K {%a}" fself x
end


let (show_a,show_b, show_all) = fix_a (new show_a_0) (new show_b_hack) (new show_all_0)
(*
let show_a_new eta =
  let (a,b, _) = fix_a (new show_a_0) (new show_b_hack) (new show_all_0) in
  a eta

let show_b_new eta =
  let (a,b, _) = fix_a (new show_a_0) (new show_b_hack) (new show_all_0) in
  a eta

let show_all2, show_b2 =
  let { show_b; show_all } = show_fix_a ~b0:({ show_b_func = new show_b_hack }) () in
  (show_all.show_all_trf, show_b.show_b_trf )

let show_b2 subj =
  let { show_b } = show_fix_a ~b0:({ show_b_func = new show_b_hack }) () in
  show_b.show_b_trf () subj*)

let _ =
  printf "Testing show_a\n";
  printf "%s\n" @@ show_a (GT.lift @@ GT.show GT.int) (E C);
  printf "%s\n" @@ show_a (GT.lift @@ GT.show GT.int) (A (I C));
  printf "Testing show_b\n";
  printf "%s\n" @@ show_b                  (I (A J));
  printf "%s\n" @@ show_b                  (K J);
  printf "Testing show_b with fixed b\n";
  printf "%s\n" @@ show_b2                 (I (A J));
  printf "%s\n" @@ show_b2                 (K J);

  printf "Testing gmap_a\n";
  printf "%s\n" @@ show_a (GT.lift @@ GT.show GT.int) @@
  gmap_a (fun () x -> x+1) (D 6);
  printf "Testing show_all with fixed b\n";
  printf "%s\n" @@ show_all                  [A(K J)];

  ()
*)
