open OUnit2
open Qbf
open Qbf.Formula
open Qbf.QFormula


(*let a,b,c = (Lit.make 1, Lit.make 2, Lit.make 3)*)
let test_quantor_false _ =
    let a,b,c = (Lit.make 1, Lit.make 2, Lit.make 3) in
    let formula = and_l [atom a; atom b; atom c] in
    let qcnf = QFormula.cnf (forall [a; b; c] (prop formula)) in
    let _ = solve ~solver:Quantor.solver qcnf
    in ()

(* Maël: I added this test after discovering a bug I had introduced when
   modifying the ./configure of quantor (line ~410). *)
let test_quantor_true _ =
    let a,b,c = (Lit.make 1, Lit.make 2, Lit.make 3) in
    let formula = or_l [atom a; atom b; atom c] in
    let qcnf = QFormula.cnf (exists [a; b; c] (prop formula)) in
    let _ = solve ~solver:Quantor.solver qcnf
    in ()


let () = run_test_tt_main (
"quantor">:::[
    "test_quantor_false">::(test_quantor_false);
    "test_quantor_true">::(test_quantor_true);
])
