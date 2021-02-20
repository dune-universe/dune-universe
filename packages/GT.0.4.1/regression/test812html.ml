type t = { a : GT.int; b : GT.string }
[@@deriving gt ~options:{html}]

type 'a t2 = A of GT.int | C of 'a * GT.int
[@@deriving gt ~options:{html}]

(* type t3 = D of t | E of GT.int t2
 * [@@deriving gt ~options:{html}] *)

type t4 = GT.int t2
[@@deriving gt ~options:{html}]

(*
let () =
  let ch = open_out "/tmp/out.html" in
  let fmt = Format.formatter_of_out_channel ch in

  let x1 = {a=5; b="beeeee"} in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  GT.html t x1;

  let x2 = A 5655 in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  GT.html t2 (GT.html GT.float) x2;

  let x3 = C (3.1415, 888) in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  GT.html t2 (GT.html GT.float) x3;

  let x4 = D x1 in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  GT.html t3 x4;

  let x5 = E (A 18) in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  GT.html t3 x5;

  Format.pp_force_newline fmt ();
  close_out ch
*)
