open ChangeRotationBasis
open UnitaryListRepresentation

module IBMGateSet =
 struct
  type coq_IBM_Unitary =
  | UIBM_U1 of float
  | UIBM_U2 of float * float
  | UIBM_U3 of float * float * float
  | UIBM_CNOT
 end

(** val coq_U1 : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_U1 a q =
  App1 ((IBMGateSet.UIBM_U1 a), q)

(** val coq_U2 :
    float -> float -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_U2 a b q =
  App1 ((IBMGateSet.UIBM_U2 (a, b)), q)

(** val coq_U3 :
    float -> float -> float -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_U3 a b c q =
  App1 ((IBMGateSet.UIBM_U3 (a, b, c)), q)

(** val coq_CNOT : int -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_CNOT q1 q2 =
  App2 (IBMGateSet.UIBM_CNOT, q1, q2)

type coq_IBM_ucom_l = IBMGateSet.coq_IBM_Unitary gate_list

(** val coq_H : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_H q =
  App1 ((IBMGateSet.UIBM_U2 ((Float.of_int 0), Float.pi)), q)

(** val coq_X : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_X q =
  App1 ((IBMGateSet.UIBM_U3 (Float.pi, (Float.of_int 0), Float.pi)), q)

(** val coq_Rz : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_Rz a q =
  App1 ((IBMGateSet.UIBM_U1 a), q)

(** val coq_T : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_T q =
  coq_Rz (( /. ) Float.pi (Float.of_int ((fun p->2*p) ((fun p->2*p) 1)))) q

(** val coq_TDAG : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_TDAG q =
  coq_Rz
    (((-.) 0.0)
      (( /. ) Float.pi (Float.of_int ((fun p->2*p) ((fun p->2*p) 1))))) q

(** val coq_P : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_P q =
  coq_Rz (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1))) q

(** val coq_PDAG : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_PDAG q =
  coq_Rz (((-.) 0.0) (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1)))) q

(** val coq_Z : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_Z q =
  coq_Rz Float.pi q

(** val coq_Y : int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_Y q =
  App1 ((IBMGateSet.UIBM_U3 (Float.pi,
    (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1))),
    (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1))))), q)

(** val coq_Rx : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_Rx a q =
  App1 ((IBMGateSet.UIBM_U3 (a,
    (((-.) 0.0) (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1)))),
    (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1))))), q)

(** val coq_Ry : float -> int -> IBMGateSet.coq_IBM_Unitary gate_app **)

let coq_Ry a q =
  App1 ((IBMGateSet.UIBM_U3 (a, (Float.of_int 0), (Float.of_int 0))), q)

(** val coq_CZ : int -> int -> coq_IBM_ucom_l **)

let coq_CZ q1 q2 =
  (coq_H q2) :: ((coq_CNOT q1 q2) :: ((coq_H q2) :: []))

(** val coq_SWAP : int -> int -> coq_IBM_ucom_l **)

let coq_SWAP q1 q2 =
  (coq_CNOT q1 q2) :: ((coq_CNOT q2 q1) :: ((coq_CNOT q1 q2) :: []))

(** val coq_CCX : int -> int -> int -> coq_IBM_ucom_l **)

let coq_CCX a b c =
  (coq_H c) :: ((coq_CNOT b c) :: ((coq_TDAG c) :: ((coq_CNOT a c) :: (
    (coq_T c) :: ((coq_CNOT b c) :: ((coq_TDAG c) :: ((coq_CNOT a c) :: (
    (coq_CNOT a b) :: ((coq_TDAG b) :: ((coq_CNOT a b) :: ((coq_T a) :: (
    (coq_T b) :: ((coq_T c) :: ((coq_H c) :: []))))))))))))))

(** val coq_CCZ : int -> int -> int -> coq_IBM_ucom_l **)

let coq_CCZ a b c =
  (coq_CNOT b c) :: ((coq_TDAG c) :: ((coq_CNOT a c) :: ((coq_T c) :: (
    (coq_CNOT b c) :: ((coq_TDAG c) :: ((coq_CNOT a c) :: ((coq_CNOT a b) :: (
    (coq_TDAG b) :: ((coq_CNOT a b) :: ((coq_T a) :: ((coq_T b) :: ((coq_T c) :: []))))))))))))

(** val compose_u3 :
    float -> float -> float -> float -> float -> float ->
    IBMGateSet.coq_IBM_Unitary **)

let compose_u3 x1 y1 z1 x2 y2 z2 =
  let (p, z) = yzy_to_zyz x1 (( +. ) y1 z2) x2 in
  let (x, y) = p in IBMGateSet.UIBM_U3 (y, (( +. ) z y2), (( +. ) z1 x))
