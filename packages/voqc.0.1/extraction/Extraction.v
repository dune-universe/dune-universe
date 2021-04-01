Require Coq.extraction.Extraction.
Require Import CXCancellation.
Require Import ChangeRotationBasis.
Require Import ConnectivityGraph.
Require Import GateCancellation.
Require Import GateSet.
Require Import HadamardReduction.
Require Import IBMGateSet.
Require Import Layouts.
Require Import Main.
Require Import NotPropagation.
Require Import Optimize1qGates.
Require Import RotationMerging.
Require Import RzQGateSet.
Require Import SimpleMapping.
Require Import StandardGateSet.
Require Import UnitaryListRepresentation.

(* Standard utilities for bools, options, etc. *)
Require Coq.extraction.ExtrOcamlBasic.

(* A few common functions not included in ExtrOcamlBasic. *)
Extract Inlined Constant fst => "fst".
Extract Inlined Constant snd => "snd".
Extract Inlined Constant negb => "not".
Extract Inlined Constant length => "List.length".
Extract Inlined Constant app => "List.append".
Extract Inlined Constant List.rev => "List.rev".
Extract Inlined Constant List.rev_append => "List.rev_append".
Extract Inlined Constant List.fold_right => "(fun f a l -> List.fold_right f l a)".
Extract Inlined Constant List.forallb => "List.for_all".
Extract Inlined Constant List.existsb => "List.exists".
Extract Inlined Constant List.filter => "List.filter".

(* Standard extraction from nat -> OCaml int and Z -> OCaml int. *)
Require Coq.extraction.ExtrOcamlNatInt.
Require Coq.extraction.ExtrOcamlZInt.

(* Inline a few operations. *)
Extraction Inline plus mult Nat.eq_dec.
Extraction Inline Z.add Z.sub Z.mul.

(* Otherwise sub will be extracted to the (undefined) string "sub". *)
Extract Inlined Constant Init.Nat.sub => "(-)".

(* Custom extraction from R -> OCaml float. *)
Extract Inlined Constant R => "float".
Extract Inlined Constant R0 => "0.0".
Extract Inlined Constant R1 => "1.0".
Extract Inlined Constant Rplus => "( +. )".
Extract Inlined Constant Rmult => "( *. )".
Extract Inlined Constant Ropp => "((-.) 0.0)".
Extract Inlined Constant Rinv => "((/.) 1.0)".
Extract Inlined Constant Rminus => "( -. )".
Extract Inlined Constant Rdiv => "( /. )".
Extract Inlined Constant cos => "cos".
Extract Inlined Constant sin => "sin".
Extract Inlined Constant tan => "tan".
Extract Inlined Constant atan => "atan".
Extract Inlined Constant acos => "acos".
Extract Inlined Constant PI => "Float.pi".
Extract Inlined Constant Reqb => "( = )".
Extract Inlined Constant Rltb => "( < )".
Extract Inlined Constant IZR => "Float.of_int".
(* Extracting the following to dummy values to supress warnings *)
Extract Constant ClassicalDedekindReals.sig_forall_dec  => "failwith ""Invalid extracted value"" ".
Extract Constant ClassicalDedekindReals.DRealRepr  => "failwith ""Invalid extracted value"" ".

(* Custom Extraction of rationals. *)
Extract Inductive Q => "Q.t" [ "" ].
Extract Inlined Constant zero_Q => "(Q.of_int 0)".
Extract Inlined Constant one_Q => "(Q.of_int 1)".
Extract Inlined Constant half_Q => "(Q.of_ints 1 2)".
Extract Inlined Constant three_halves_Q => "(Q.of_ints 3 2)".
Extract Inlined Constant quarter_Q => "(Q.of_ints 1 4)".
Extract Inlined Constant seven_quarters_Q => "(Q.of_ints 7 4)".
Extract Inlined Constant two_Q => "(Q.of_int 2)".
Extract Inlined Constant Qplus => "Q.add".
Extract Inlined Constant Qminus => "Q.sub".
Extract Inlined Constant Qmult => "Q.mul".
Extract Inlined Constant Qeq_bool => "Q.equal".
Extract Inlined Constant Qle_bool => "Q.leq".
Extract Inlined Constant inject_Z => "Q.of_int".
Extract Inlined Constant Qnum => "(fun q -> Z.to_int (Q.num q))".
Extract Inlined Constant Qden => "(fun q -> Z.to_int (Q.den q))".    
(* It's easier to extract these functions by hand.
   bound is used in RzQGateSet; it puts a rational q in the range [0,2) *)
Extract Constant RzQGateSet.bound => 
  "let round_to_multiple_of_2 q = 
      let num = Q.num q in 
      let den = Q.den q in
      Z.mul (Z.of_int 2) (Z.div num (Z.mul den (Z.of_int 2))) in
   fun q ->
   if (Q.leq (Q.of_int 0) q) && not (Q.leq (Q.of_int 2) q) then q
   else if (Q.leq (Q.of_int 2) q)
        then Q.sub q (Q.of_bigint (round_to_multiple_of_2 q))
        else Q.add q (Q.of_bigint (round_to_multiple_of_2 q))".
Extract Inlined Constant R2Q => "Q.of_float".

(* Set the dimension argument to be implicit everywhere -- it should be an
   unused argument everywhere in the OCaml code. *)

(* From ListRepresentation.v *)
Extraction Implicit next_single_qubit_gate' [dim].
Extraction Implicit next_single_qubit_gate [dim].
Extraction Implicit last_single_qubit_gate [dim].
Extraction Implicit next_two_qubit_gate' [dim].
Extraction Implicit next_two_qubit_gate [dim].
Extraction Implicit next_gate' [dim].
Extraction Implicit next_gate [dim].
Extraction Implicit does_not_reference_appl [dim].
Extraction Implicit does_not_reference [dim].
Extraction Implicit UnitaryListRepresentation.remove_prefix [dim].
Extraction Implicit UnitaryListRepresentation.remove_suffix [dim].
Extraction Implicit UnitaryListRepresentation.replace_pattern [dim].
Extraction Implicit try_rewrites [dim].
Extraction Implicit try_rewrites2 [dim].
Extraction Implicit propagate' [dim].
Extraction Implicit propagate [dim].
Extraction Implicit get_matching_prefix' [dim].
Extraction Implicit get_matching_prefix [dim].
Extraction Implicit LCR [dim].

(* From RzQGateSet.v *)
Extraction Implicit RzQGateSet.Rzq [dim].
Extraction Implicit RzQGateSet.H [dim].
Extraction Implicit RzQGateSet.X [dim].
Extraction Implicit RzQGateSet.T [dim].
Extraction Implicit RzQGateSet.TDAG [dim].
Extraction Implicit RzQGateSet.P [dim].
Extraction Implicit RzQGateSet.PDAG [dim].
Extraction Implicit RzQGateSet.Z [dim].
Extraction Implicit RzQGateSet.CNOT [dim].
Extraction Implicit RzQGateSet.Y [dim].
Extraction Implicit RzQGateSet.CZ [dim].
Extraction Implicit RzQGateSet.SWAP [dim].
Extraction Implicit RzQGateSet.CCX [dim].
Extraction Implicit RzQGateSet.CCZ [dim].
Extraction Implicit RzQGateSet.combine_rotations [dim].
Extraction Implicit RzQGateSet.invert_rotation [dim].
Extraction Implicit RzQGateSet.remove_prefix [dim].
Extraction Implicit RzQGateSet.remove_suffix [dim].
Extraction Implicit RzQGateSet.replace_pattern [dim].

(* From HadamardReduction.v *)
Extraction Implicit HadamardReduction.apply_H_equivalence1 [dim].
Extraction Implicit HadamardReduction.apply_H_equivalence2 [dim].
Extraction Implicit HadamardReduction.apply_H_equivalence3 [dim].
Extraction Implicit HadamardReduction.apply_H_equivalence4 [dim].
Extraction Implicit HadamardReduction.apply_H_equivalence5 [dim].
Extraction Implicit HadamardReduction.apply_H_equivalence [dim].
Extraction Implicit HadamardReduction.apply_H_equivalences' [dim].
Extraction Implicit HadamardReduction.hadamard_reduction [dim].

(* From GateCancellation.v *)
Extraction Implicit GateCancellation.Rz_commute_rule1 [dim].
Extraction Implicit GateCancellation.Rz_commute_rule2 [dim].
Extraction Implicit GateCancellation.Rz_commute_rule3 [dim].
Extraction Implicit GateCancellation.Rz_commute_rules [dim].
Extraction Implicit GateCancellation.Rz_cancel_rule [dim].
Extraction Implicit GateCancellation.H_cancel_rule [dim].
Extraction Implicit GateCancellation.X_commute_rule [dim].
Extraction Implicit GateCancellation.X_cancel_rule [dim].
Extraction Implicit GateCancellation.CNOT_commute_rule1 [dim].
Extraction Implicit GateCancellation.CNOT_commute_rule2 [dim].
Extraction Implicit GateCancellation.CNOT_commute_rule3 [dim].
Extraction Implicit GateCancellation.CNOT_commute_rule4 [dim].
Extraction Implicit GateCancellation.CNOT_commute_rule5 [dim].
Extraction Implicit GateCancellation.CNOT_commute_rules [dim].
Extraction Implicit GateCancellation.CNOT_cancel_rule [dim].
Extraction Implicit GateCancellation.propagate_Rz [dim].
Extraction Implicit GateCancellation.propagate_H [dim].
Extraction Implicit GateCancellation.propagate_X [dim].
Extraction Implicit GateCancellation.propagate_CNOT [dim].
Extraction Implicit GateCancellation.cancel_single_qubit_gates' [dim].
Extraction Implicit GateCancellation.cancel_single_qubit_gates [dim].
Extraction Implicit GateCancellation.cancel_two_qubit_gates' [dim].
Extraction Implicit GateCancellation.cancel_two_qubit_gates [dim].

(* From RotationMerging.v *)
Extraction Implicit RotationMerging.find_merge' [dim].
Extraction Implicit RotationMerging.find_merge [dim].
Extraction Implicit RotationMerging.merge_at_beginning [dim].
Extraction Implicit RotationMerging.merge_at_end [dim].
Extraction Implicit RotationMerging.merge_rotations_at_beginning [dim].
Extraction Implicit RotationMerging.merge_rotations_at_end [dim].
Extraction Implicit RotationMerging.invert_gate [dim].
Extraction Implicit RotationMerging.invert [dim].
Extraction Implicit RotationMerging.merge_rotations [dim].

(* From NotPropagation.v *)
Extraction Implicit NotPropagation.finalize [dim].
Extraction Implicit NotPropagation.not_propagation' [dim].
Extraction Implicit NotPropagation.not_propagation [dim].

(* From IBMGateSet.v *)
Extraction Implicit IBMGateSet.U1 [dim].
Extraction Implicit IBMGateSet.U2 [dim].
Extraction Implicit IBMGateSet.U3 [dim].
Extraction Implicit IBMGateSet.CNOT [dim].
Extraction Implicit IBMGateSet.H [dim].
Extraction Implicit IBMGateSet.X [dim].
Extraction Implicit IBMGateSet.Rz [dim].
Extraction Implicit IBMGateSet.T [dim].
Extraction Implicit IBMGateSet.TDAG [dim].
Extraction Implicit IBMGateSet.P [dim].
Extraction Implicit IBMGateSet.PDAG [dim].
Extraction Implicit IBMGateSet.Z [dim].
Extraction Implicit IBMGateSet.Y [dim].
Extraction Implicit IBMGateSet.Rx [dim].
Extraction Implicit IBMGateSet.Ry [dim].
Extraction Implicit IBMGateSet.CZ [dim].
Extraction Implicit IBMGateSet.SWAP [dim].
Extraction Implicit IBMGateSet.CCX [dim].
Extraction Implicit IBMGateSet.CCZ [dim].

(* From Optimize1qGates.v *)
Extraction Implicit Optimize1qGates.optimize_1q_gates' [dim].
Extraction Implicit Optimize1qGates.simplify_1q_gates [dim].
Extraction Implicit Optimize1qGates.optimize_1q_gates [dim].

(* From CXCancellation.v *)
Extraction Implicit CXCancellation.cx_cancellation' [dim].
Extraction Implicit CXCancellation.cx_cancellation [dim].

(* From StandardGateSet.v *)
Extraction Implicit StandardGateSet.change_gate_set' [dim].
Extraction Implicit StandardGateSet.change_gate_set [dim].
Extraction Implicit StandardGateSet.standard_to_IBM_u [dim].
Extraction Implicit StandardGateSet.IBM_to_standard_u [dim].
Extraction Implicit StandardGateSet.standard_to_IBM [dim].
Extraction Implicit StandardGateSet.IBM_to_standard [dim].
Extraction Implicit StandardGateSet.Rx [dim].
Extraction Implicit StandardGateSet.Ry [dim].
Extraction Implicit StandardGateSet.Rz [dim].
Extraction Implicit StandardGateSet.U1 [dim].
Extraction Implicit StandardGateSet.U2 [dim].
Extraction Implicit StandardGateSet.U3 [dim].
Extraction Implicit StandardGateSet.standard_to_RzQ_u [dim].
Extraction Implicit StandardGateSet.RzQ_to_standard_u [dim].
Extraction Implicit StandardGateSet.standard_to_RzQ [dim].
Extraction Implicit StandardGateSet.RzQ_to_standard [dim].
Extraction Implicit StandardGateSet.decompose_to_cnot_u [dim].
Extraction Implicit StandardGateSet.decompose_to_cnot [dim].
Extraction Implicit StandardGateSet.convert_to_ibm [dim].
Extraction Implicit StandardGateSet.convert_to_rzq [dim].
Extraction Implicit StandardGateSet.replace_rzq_u [dim].
Extraction Implicit StandardGateSet.replace_rzq [dim].

(* From Layouts.v *)
Extraction Implicit Layouts.log2phys [dim].
Extraction Implicit Layouts.phys2log [dim].
Extraction Implicit Layouts.swap_in_map [dim].

(* From SimpleMapping.v *)
Extraction Implicit SimpleMapping.respects_constraints_directed_b [dim].
Extraction Implicit SimpleMapping.decompose_swaps_u [dim].
Extraction Implicit SimpleMapping.decompose_swaps [dim].
Extraction Implicit SimpleMapping.SWAP [dim].
Extraction Implicit SimpleMapping.CNOT [dim].
Extraction Implicit SimpleMapping.H [dim].
Extraction Implicit SimpleMapping.path_to_swaps [dim].
Extraction Implicit SimpleMapping.fix_cnots [dim].
Extraction Implicit SimpleMapping.insert_swaps [dim].
Extraction Implicit SimpleMapping.simple_map [dim].

(* From Main.v *)
Extraction Implicit Main.cast [dim].
Extract Inlined Constant Main.cast => "(fun x _ -> x)".
Extraction Implicit Main.cast_layout [dim].
Extract Inlined Constant Main.cast_layout => "(fun x _ -> x)".
Extraction Implicit Main.check_well_typed [dim].
Extraction Implicit Main.convert_to_ibm [dim].
Extraction Implicit Main.convert_to_rzq [dim].
Extraction Implicit Main.replace_rzq [dim].
Extraction Implicit Main.decompose_to_cnot [dim].
Extraction Implicit Main.count_I [dim].
Extraction Implicit Main.count_X [dim].
Extraction Implicit Main.count_Y [dim].
Extraction Implicit Main.count_Z [dim].
Extraction Implicit Main.count_H [dim].
Extraction Implicit Main.count_S [dim].
Extraction Implicit Main.count_T [dim].
Extraction Implicit Main.count_Sdg [dim].
Extraction Implicit Main.count_Tdg [dim].
Extraction Implicit Main.count_Rx [dim].
Extraction Implicit Main.count_Ry [dim].
Extraction Implicit Main.count_Rz [dim].
Extraction Implicit Main.count_Rzq [dim].
Extraction Implicit Main.count_U1 [dim].
Extraction Implicit Main.count_U2 [dim].
Extraction Implicit Main.count_U3 [dim].
Extraction Implicit Main.count_CX [dim].
Extraction Implicit Main.count_CZ [dim].
Extraction Implicit Main.count_SWAP [dim].
Extraction Implicit Main.count_CCX [dim].
Extraction Implicit Main.count_CCZ [dim].
Extraction Implicit Main.count_gates [dim].
Extraction Implicit Main.total_gate_count [dim].
Extraction Implicit Main.count_clifford_rzq [dim].
Extraction Implicit Main.count_gates_lcr [dim]. 
Extraction Implicit Main.optimize_1q_gates [dim].
Extraction Implicit Main.cx_cancellation [dim].
Extraction Implicit Main.optimize_ibm [dim].
Extraction Implicit Main.not_propagation [dim].
Extraction Implicit Main.hadamard_reduction [dim].
Extraction Implicit Main.cancel_single_qubit_gates [dim].
Extraction Implicit Main.cancel_two_qubit_gates [dim].
Extraction Implicit Main.merge_rotations [dim].
Extraction Implicit Main.optimize_nam [dim].
Extraction Implicit Main.optimize_nam_light [dim].
Extraction Implicit Main.optimize_nam_lcr [dim].
Extraction Implicit Main.check_layout [dim].
Extraction Implicit Main.check_constraints [dim].
Extraction Implicit Main.simple_map [cdim ldim].
Extraction Implicit Main.layout_to_list [dim].

(* Perform extraction. *)
Separate Extraction
  Main.check_well_typed Main.convert_to_ibm Main.convert_to_rzq 
  Main.replace_rzq Main.decompose_to_cnot Main.count_gates Main.total_gate_count
  Main.count_clifford_rzq Main.count_gates_lcr Main.optimize_ibm
  Main.optimize_nam Main.optimize_nam_light Main.optimize_nam_lcr
  Main.check_layout Main.check_graph Main.check_constraints Main.simple_map
  Main.make_tenerife Main.make_lnn Main.make_lnn_ring Main.make_grid
  Main.trivial_layout Main.list_to_layout Main.layout_to_list.
