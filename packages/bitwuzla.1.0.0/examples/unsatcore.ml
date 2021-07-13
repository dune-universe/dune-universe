let () =

  (* First, create a Bitwuzla instance enabling unsat core extraction. *)
  let open Bitwuzla.Unsat_core () in

  (* Create a bit-vector sort of size 2 and another of size 4. *)
  let bv2 = Sort.bv 2 and bv4 = Sort.bv 4
  (* Create Float16 floatinf-point sort. *)
  and fp16 = Sort.fp ~exponent:5 16 in

  (* Create bit-vector variables. *)
  (* (declare-const x0 (_ BitVec 4)) *)
  let x0 = Term.const bv4 "x0"
  (* (declare-const x1 (_ BitVec 2)) *)
  and x1 = Term.const bv2 "x1"
  (* (declare-const x2 (_ BitVec 2)) *)
  and x2 = Term.const bv2 "x2"
  (* (declare-const x3 (_ BitVec 2)) *)
  and x3 = Term.const bv2 "x3"
  (* (declare-const x4 Float16) *)
  and x4 = Term.const fp16 "x4" in

  (* Create FP positive zero. *)
  let fpzero = Term.Fp.pos_zero fp16
  (* Create BV zero of size 4. *)
  and bvzero = Term.Bv.zero bv4 in

  (* (define-fun f0 ((a Float16)) Bool (fp.gt a (_ +zero 5 11))) *)
  let f0 = Term.Uf.lambda [ fp16 ] (fun [ a ] -> Term.Fp.gt a fpzero) in

  (* (define-fun f1 ((a Float16)) (_ BitVec 4) (ite (f0 a) x0 #b0000)) *)
  let f1 = Term.Uf.lambda [ fp16 ] (fun [ a ] ->
      Term.ite (Term.Uf.apply f0 [ a ]) x0 bvzero) in

  (* (define-fun f2 ((a Float16)) (_ BitVec 2) ((_ extract 1 0) (f1 a))) *)
  let f2 = Term.Uf.lambda [ fp16 ] (fun [ a ] ->
      Term.Bv.extract ~hi:1 ~lo:0 (Term.Uf.apply f1 [ a ])) in

  (* (assert (! (bvult x2 (f2 (_ +zero 5 11))) :named assertion0)) *)
  assert' ~name:"assertion0"
  @@ Term.Bv.ult x2 (Term.Uf.apply f2 [ fpzero ]);

  (* (assert (! (= x1 x2 x3) :named assertion1)) *)
  assert' ~name:"assertion1"
  @@ Term.Bl.logand (Term.equal x1 x2) (Term.equal x2 x3);

  (* (assert (!(= x4 ((_ to_fp_unsigned 5 11) RNE x3)) :named assertion2)) *)
  assert' ~name:"assertion2"
  @@ Term.equal x4 (Term.Fp.of_sbv ~exponent:5 16 Term.Rm.rne x3);

  (* (check-sat) *)
  Format.printf "Expect: unsat@\n";
  Format.printf "Bitwuzla: %a@\n" pp_result @@ check_sat ();

  (* (get-unsat-core) *)
  Format.printf "Unsat core: {";
  Array.iter (fun a -> Format.printf " %a" Term.pp a) @@ get_unsat_core ();
  Format.printf " }@\n";

  (* Finally, delete the Bitwuzla instance. *)
  unsafe_close ()
