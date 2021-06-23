open UnitaryListRepresentation

module IBMGateSet :
 sig
  type coq_IBM_Unitary =
  | UIBM_U1 of float
  | UIBM_U2 of float * float
  | UIBM_U3 of float * float * float
  | UIBM_CNOT
 end

val coq_U1 : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_U2 : float -> float -> int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_U3 :
  float -> float -> float -> int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_CNOT : int -> int -> IBMGateSet.coq_IBM_Unitary gate_app

type coq_IBM_ucom_l = IBMGateSet.coq_IBM_Unitary gate_list

val coq_H : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_X : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_Rz : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_T : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_TDAG : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_P : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_PDAG : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_Z : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_Y : int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_Rx : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_Ry : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app

val coq_CZ : int -> int -> coq_IBM_ucom_l

val coq_SWAP : int -> int -> coq_IBM_ucom_l

val coq_CCX : int -> int -> int -> coq_IBM_ucom_l

val coq_CCZ : int -> int -> int -> coq_IBM_ucom_l

val compose_u3 :
  float -> float -> float -> float -> float -> float ->
  IBMGateSet.coq_IBM_Unitary
