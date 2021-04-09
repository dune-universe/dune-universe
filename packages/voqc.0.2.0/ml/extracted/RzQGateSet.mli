open UnitaryListRepresentation

module RzQGateSet :
 sig
  type coq_RzQ_Unitary =
  | URzQ_H
  | URzQ_X
  | URzQ_Rz of Q.t
  | URzQ_CNOT

  type coq_U = coq_RzQ_Unitary

  val match_gate : int -> coq_U -> coq_U -> bool
 end

val coq_Rzq : Q.t -> int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_H : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_X : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_T : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_TDAG : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_P : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_PDAG : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_Z : int -> RzQGateSet.coq_RzQ_Unitary gate_app

val coq_CNOT : int -> int -> RzQGateSet.coq_RzQ_Unitary gate_app

type coq_RzQ_ucom_l = RzQGateSet.coq_RzQ_Unitary gate_list

val coq_Y : int -> coq_RzQ_ucom_l

val coq_CZ : int -> int -> coq_RzQ_ucom_l

val coq_SWAP : int -> int -> coq_RzQ_ucom_l

val coq_CCX : int -> int -> int -> coq_RzQ_ucom_l

val coq_CCZ : int -> int -> int -> coq_RzQ_ucom_l

val remove_prefix :
  coq_RzQ_ucom_l -> coq_RzQ_ucom_l -> RzQGateSet.coq_RzQ_Unitary gate_list
  option

val replace_pattern :
  coq_RzQ_ucom_l -> coq_RzQ_ucom_l -> coq_RzQ_ucom_l ->
  RzQGateSet.coq_RzQ_Unitary gate_list option

val bound : Q.t -> Q.t

val combine_rotations : Q.t -> Q.t -> int -> coq_RzQ_ucom_l

val invert_rotation : Q.t -> int -> RzQGateSet.coq_RzQ_Unitary gate_app
