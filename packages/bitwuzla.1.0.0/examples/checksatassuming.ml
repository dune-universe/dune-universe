let () =

  (* First, create a Bitwuzla instance enabling incremental solving. *)
  let open Bitwuzla.Incremental () in

  (* Create a bit-vector sort of size 1 and another of size 2. *)
  let bv1 = Sort.bv 1 and bv2 = Sort.bv 2 in

  (* (declare-const o0 (_ BitVec 1)) *)
  let o0 = Term.const bv1 "o0"
  (* (declare-const o1 (_ BitVec 1)) *)
  and o1 = Term.const bv1 "o1"
  (* (declare-const s0 (_ BitVec 2)) *)
  and s0 = Term.const bv2 "s0"
  (* (declare-const s1 (_ BitVec 2)) *)
  and s1 = Term.const bv2 "s1"
  (* (declare-const s2 (_ BitVec 2)) *)
  and s2 = Term.const bv2 "s2"
  (* (declare-const goal (_ BitVec 2)) *)
  and goal  = Term.const bv2 "goal"

  (* Create bit-vector values zero, one, three. *)
  and zero  = Term.Bv.zero bv2
  and one2  = Term.Bv.one bv2
  and three = Term.Bv.of_int bv2 3 in

  (* Add some assertions. *)
  assert' @@ Term.equal s0 zero;
  assert' @@ Term.equal goal three;

  (* (check-sat-assuming ((= s0 goal))) *)
  Format.printf "Expect: unsat@\n";
  Format.printf "Bitwuzla: %a@\n" pp_result
  @@ check_sat_assuming [| Term.equal s0 goal |];

  (* (assert (= s1 (ite (= o0 (_ sortbv1 1)) (bvadd s0 one) s0))) *)
  assert' @@ Term.equal s1 (Term.ite o0 (Term.Bv.add s0 one2) s0);

  (* (check-sat-assuming ((= s1 goal))) *)
  Format.printf "Expect: unsat@\n";
  Format.printf "Bitwuzla: %a@\n" pp_result
  @@ check_sat_assuming [| Term.equal s1 goal |];

  (* (assert (= s2 (ite (= o1 (_ sortbv1 1)) (bvadd s1 one) s1))) *)
  assert' @@ Term.equal s2 (Term.ite o1 (Term.Bv.add s1 one2) s1);

  (* (check-sat-assuming ((= s2 goal))) *)
  Format.printf "Expect: unsat@\n";
  Format.printf "Bitwuzla: %a@\n" pp_result
  @@ check_sat_assuming [| Term.equal s2 goal |];

  (* Finally, delete the Bitwuzla instance. *)
  unsafe_close ()
