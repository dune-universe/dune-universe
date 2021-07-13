let () =

  (* First, create a Bitwuzla instance. *)
  let open Bitwuzla.Once () in

  (* Create a bit-vector sort of size 8. *)
  let bv8 = Sort.bv 8 in

  (* Create two bit-vector variables of that sort. *)
  let x = Term.const bv8 "x" and y = Term.const bv8 "y" in

  (* Create bit-vector values one and two of the same sort. *)
  let one = Term.Bv.one bv8 and two = Term.Bv.of_int bv8 2 in

  (* (bvsdiv x (_ bv2 8)) *)
  let sdiv = Term.Bv.sdiv x two in

  (* (bvashr y (_ bv1 8)) *)
  let ashr = Term.Bv.shift_right y one in

  (* ((_ extract 3 0) (bvsdiv x (_ bv2 8))) *)
  let sdive = Term.Bv.extract ~hi:3 ~lo:0 sdiv in

  (* ((_ extract 3 0) (bvashr x (_ sortbv1 8))) *)
  let ashre = Term.Bv.extract ~hi:3 ~lo:0 ashr in

  (*
     (assert
       (distinct
         ((_ extract 3 0) (bvsdiv x (_ sortbv2 8)))
         ((_ extract 3 0) (bvashr y (_ sortbv1 8)))))
  *)
  assert' @@ Term.distinct sdive ashre;

  (* (check-sat) *)
  let result = check_sat () in
  assert (result = Sat);

  (* (get-model) *)
  let xval = get_value x and yval = get_value y in
  Format.printf "assignment of x: %s@\n" @@ Z.format "%08b"
  @@ Term.Bv.assignment xval;
  Format.printf "assignment of y: %s@\n" @@ Z.format "%08b"
  @@ Term.Bv.assignment yval;
  let x2 = Term.Bv.mul x x in
  let x2val = get_value x2 in
  Format.printf "assignment of x * x: %s@\n" @@ Z.format "%08b"
  @@ Term.Bv.assignment x2val;

  (* Finally, delete the Bitwuzla instance. *)
  unsafe_close ()
