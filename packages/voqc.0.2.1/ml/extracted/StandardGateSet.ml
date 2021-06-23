open UnitaryListRepresentation

module StandardGateSet =
 struct
  type coq_Std_Unitary =
  | U_I
  | U_X
  | U_Y
  | U_Z
  | U_H
  | U_S
  | U_T
  | U_Sdg
  | U_Tdg
  | U_Rx of float
  | U_Ry of float
  | U_Rz of float
  | U_Rzq of Q.t
  | U_U1 of float
  | U_U2 of float * float
  | U_U3 of float * float * float
  | U_CX
  | U_CZ
  | U_SWAP
  | U_CCX
  | U_CCZ

  type coq_U = coq_Std_Unitary

  (** val match_gate : int -> coq_U -> coq_U -> bool **)

  let match_gate _ u u' =
    match u with
    | U_I -> (match u' with
              | U_I -> true
              | _ -> false)
    | U_X -> (match u' with
              | U_X -> true
              | _ -> false)
    | U_Y -> (match u' with
              | U_Y -> true
              | _ -> false)
    | U_Z -> (match u' with
              | U_Z -> true
              | _ -> false)
    | U_H -> (match u' with
              | U_H -> true
              | _ -> false)
    | U_S -> (match u' with
              | U_S -> true
              | _ -> false)
    | U_T -> (match u' with
              | U_T -> true
              | _ -> false)
    | U_Sdg -> (match u' with
                | U_Sdg -> true
                | _ -> false)
    | U_Tdg -> (match u' with
                | U_Tdg -> true
                | _ -> false)
    | U_Rx r -> (match u' with
                 | U_Rx r' -> ( = ) r r'
                 | _ -> false)
    | U_Ry r -> (match u' with
                 | U_Ry r' -> ( = ) r r'
                 | _ -> false)
    | U_Rz r -> (match u' with
                 | U_Rz r' -> ( = ) r r'
                 | _ -> false)
    | U_Rzq q -> (match u' with
                  | U_Rzq q' -> Q.equal q q'
                  | _ -> false)
    | U_U1 r -> (match u' with
                 | U_U1 r' -> ( = ) r r'
                 | _ -> false)
    | U_U2 (r1, r2) ->
      (match u' with
       | U_U2 (r1', r2') -> (&&) (( = ) r1 r1') (( = ) r2 r2')
       | _ -> false)
    | U_U3 (r1, r2, r3) ->
      (match u' with
       | U_U3 (r1', r2', r3') ->
         (&&) ((&&) (( = ) r1 r1') (( = ) r2 r2')) (( = ) r3 r3')
       | _ -> false)
    | U_CX -> (match u' with
               | U_CX -> true
               | _ -> false)
    | U_CZ -> (match u' with
               | U_CZ -> true
               | _ -> false)
    | U_SWAP -> (match u' with
                 | U_SWAP -> true
                 | _ -> false)
    | U_CCX -> (match u' with
                | U_CCX -> true
                | _ -> false)
    | U_CCZ -> (match u' with
                | U_CCZ -> true
                | _ -> false)
 end

type standard_ucom_l = StandardGateSet.coq_Std_Unitary gate_list

(** val change_gate_set' :
    ('a1 gate_app -> 'a2 gate_list) -> 'a1 gate_list -> 'a2 gate_list -> 'a2
    gate_list **)

let rec change_gate_set' f l acc =
  match l with
  | [] -> List.rev acc
  | g :: t -> change_gate_set' f t (List.append (List.rev (f g)) acc)

(** val change_gate_set :
    ('a1 gate_app -> 'a2 gate_list) -> 'a1 gate_list -> 'a2 gate_list **)

let change_gate_set f l =
  change_gate_set' f l []

(** val standard_to_IBM_u :
    StandardGateSet.coq_Std_Unitary gate_app -> IBMGateSet.coq_IBM_ucom_l **)

let standard_to_IBM_u = function
| App1 (s, m) ->
  (match s with
   | StandardGateSet.U_I -> (IBMGateSet.coq_Rz (Float.of_int 0) m) :: []
   | StandardGateSet.U_X -> (IBMGateSet.coq_X m) :: []
   | StandardGateSet.U_Y -> (IBMGateSet.coq_Y m) :: []
   | StandardGateSet.U_Z -> (IBMGateSet.coq_Z m) :: []
   | StandardGateSet.U_H -> (IBMGateSet.coq_H m) :: []
   | StandardGateSet.U_S -> (IBMGateSet.coq_P m) :: []
   | StandardGateSet.U_T -> (IBMGateSet.coq_T m) :: []
   | StandardGateSet.U_Sdg -> (IBMGateSet.coq_PDAG m) :: []
   | StandardGateSet.U_Tdg -> (IBMGateSet.coq_TDAG m) :: []
   | StandardGateSet.U_Rx r -> (IBMGateSet.coq_Rx r m) :: []
   | StandardGateSet.U_Ry r -> (IBMGateSet.coq_Ry r m) :: []
   | StandardGateSet.U_Rz r -> (IBMGateSet.coq_Rz r m) :: []
   | StandardGateSet.U_Rzq q ->
     (IBMGateSet.coq_Rz (( *. ) (Q.to_float q) Float.pi) m) :: []
   | StandardGateSet.U_U1 r -> (IBMGateSet.coq_U1 r m) :: []
   | StandardGateSet.U_U2 (r1, r2) -> (IBMGateSet.coq_U2 r1 r2 m) :: []
   | StandardGateSet.U_U3 (r1, r2, r3) -> (IBMGateSet.coq_U3 r1 r2 r3 m) :: []
   | _ -> [])
| App2 (s, m, n) ->
  (match s with
   | StandardGateSet.U_CX -> (IBMGateSet.coq_CNOT m n) :: []
   | StandardGateSet.U_CZ -> IBMGateSet.coq_CZ m n
   | StandardGateSet.U_SWAP -> IBMGateSet.coq_SWAP m n
   | _ -> [])
| App3 (s, m, n, p) ->
  (match s with
   | StandardGateSet.U_CCX -> IBMGateSet.coq_CCX m n p
   | StandardGateSet.U_CCZ -> IBMGateSet.coq_CCZ m n p
   | _ -> [])

(** val coq_IBM_to_standard_u :
    IBMGateSet.IBMGateSet.coq_IBM_Unitary gate_app -> standard_ucom_l **)

let coq_IBM_to_standard_u = function
| App1 (i, m) ->
  (match i with
   | IBMGateSet.IBMGateSet.UIBM_U1 a ->
     (App1 ((StandardGateSet.U_U1 a), m)) :: []
   | IBMGateSet.IBMGateSet.UIBM_U2 (a, b) ->
     (App1 ((StandardGateSet.U_U2 (a, b)), m)) :: []
   | IBMGateSet.IBMGateSet.UIBM_U3 (a, b, c) ->
     (App1 ((StandardGateSet.U_U3 (a, b, c)), m)) :: []
   | IBMGateSet.IBMGateSet.UIBM_CNOT -> [])
| App2 (i, m, n) ->
  (match i with
   | IBMGateSet.IBMGateSet.UIBM_CNOT ->
     (App2 (StandardGateSet.U_CX, m, n)) :: []
   | _ -> [])
| App3 (_, _, _, _) -> []

(** val standard_to_IBM : standard_ucom_l -> IBMGateSet.coq_IBM_ucom_l **)

let standard_to_IBM l =
  change_gate_set standard_to_IBM_u l

(** val coq_IBM_to_standard : IBMGateSet.coq_IBM_ucom_l -> standard_ucom_l **)

let coq_IBM_to_standard l =
  change_gate_set coq_IBM_to_standard_u l

(** val coq_R2Q_PI : float -> Q.t **)

let coq_R2Q_PI x =
  Q.of_float (( /. ) x Float.pi)

(** val coq_Rx : float -> int -> RzQGateSet.coq_RzQ_ucom_l **)

let coq_Rx a q =
  (RzQGateSet.coq_H q) :: ((RzQGateSet.coq_Rzq (coq_R2Q_PI a) q) :: (
    (RzQGateSet.coq_H q) :: []))

(** val coq_Rz :
    float -> int -> RzQGateSet.RzQGateSet.coq_RzQ_Unitary gate_app **)

let coq_Rz a q =
  App1 ((RzQGateSet.RzQGateSet.URzQ_Rz (coq_R2Q_PI a)), q)

(** val coq_Ry : float -> int -> RzQGateSet.coq_RzQ_ucom_l **)

let coq_Ry a q =
  (RzQGateSet.coq_PDAG q) :: ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_Rzq
                                                         (coq_R2Q_PI a) q) :: (
    (RzQGateSet.coq_H q) :: ((RzQGateSet.coq_P q) :: []))))

(** val coq_U1 :
    float -> int -> RzQGateSet.RzQGateSet.coq_RzQ_Unitary gate_app **)

let coq_U1 =
  coq_Rz

(** val coq_U2 : float -> float -> int -> RzQGateSet.coq_RzQ_ucom_l **)

let coq_U2 a b q =
  (RzQGateSet.coq_Rzq (coq_R2Q_PI (( -. ) b Float.pi)) q) :: ((RzQGateSet.coq_H
                                                                q) :: (
    (RzQGateSet.coq_Rzq (coq_R2Q_PI a) q) :: []))

(** val coq_U3 :
    float -> float -> float -> int -> RzQGateSet.coq_RzQ_ucom_l **)

let coq_U3 a b c q =
  (RzQGateSet.coq_Rzq
    (coq_R2Q_PI (( -. ) c (( /. ) Float.pi (Float.of_int ((fun p->2*p) 1)))))
    q) :: ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_Rzq (coq_R2Q_PI a) q) :: (
    (RzQGateSet.coq_H q) :: ((RzQGateSet.coq_Rzq
                               (coq_R2Q_PI
                                 (( +. ) b
                                   (( /. ) Float.pi
                                     (Float.of_int ((fun p->2*p) 1))))) q) :: []))))

(** val standard_to_RzQ_u :
    StandardGateSet.coq_Std_Unitary gate_app -> RzQGateSet.coq_RzQ_ucom_l **)

let standard_to_RzQ_u = function
| App1 (s, m) ->
  (match s with
   | StandardGateSet.U_I -> (RzQGateSet.coq_Rzq (Q.of_int 0) m) :: []
   | StandardGateSet.U_X -> (RzQGateSet.coq_X m) :: []
   | StandardGateSet.U_Y -> RzQGateSet.coq_Y m
   | StandardGateSet.U_Z -> (RzQGateSet.coq_Z m) :: []
   | StandardGateSet.U_H -> (RzQGateSet.coq_H m) :: []
   | StandardGateSet.U_S -> (RzQGateSet.coq_P m) :: []
   | StandardGateSet.U_T -> (RzQGateSet.coq_T m) :: []
   | StandardGateSet.U_Sdg -> (RzQGateSet.coq_PDAG m) :: []
   | StandardGateSet.U_Tdg -> (RzQGateSet.coq_TDAG m) :: []
   | StandardGateSet.U_Rx r -> coq_Rx r m
   | StandardGateSet.U_Ry r -> coq_Ry r m
   | StandardGateSet.U_Rz r -> (coq_Rz r m) :: []
   | StandardGateSet.U_Rzq q -> (RzQGateSet.coq_Rzq q m) :: []
   | StandardGateSet.U_U1 r -> (coq_U1 r m) :: []
   | StandardGateSet.U_U2 (r1, r2) -> coq_U2 r1 r2 m
   | StandardGateSet.U_U3 (r1, r2, r3) -> coq_U3 r1 r2 r3 m
   | _ -> [])
| App2 (s, m, n) ->
  (match s with
   | StandardGateSet.U_CX -> (RzQGateSet.coq_CNOT m n) :: []
   | StandardGateSet.U_CZ -> RzQGateSet.coq_CZ m n
   | StandardGateSet.U_SWAP -> RzQGateSet.coq_SWAP m n
   | _ -> [])
| App3 (s, m, n, p) ->
  (match s with
   | StandardGateSet.U_CCX -> RzQGateSet.coq_CCX m n p
   | StandardGateSet.U_CCZ -> RzQGateSet.coq_CCZ m n p
   | _ -> [])

(** val coq_RzQ_to_standard_u :
    RzQGateSet.RzQGateSet.coq_RzQ_Unitary gate_app -> standard_ucom_l **)

let coq_RzQ_to_standard_u = function
| App1 (r, m) ->
  (match r with
   | RzQGateSet.RzQGateSet.URzQ_H -> (App1 (StandardGateSet.U_H, m)) :: []
   | RzQGateSet.RzQGateSet.URzQ_X -> (App1 (StandardGateSet.U_X, m)) :: []
   | RzQGateSet.RzQGateSet.URzQ_Rz q ->
     (App1 ((StandardGateSet.U_Rzq q), m)) :: []
   | RzQGateSet.RzQGateSet.URzQ_CNOT -> [])
| App2 (r, m, n) ->
  (match r with
   | RzQGateSet.RzQGateSet.URzQ_CNOT ->
     (App2 (StandardGateSet.U_CX, m, n)) :: []
   | _ -> [])
| App3 (_, _, _, _) -> []

(** val standard_to_RzQ : standard_ucom_l -> RzQGateSet.coq_RzQ_ucom_l **)

let standard_to_RzQ l =
  change_gate_set standard_to_RzQ_u l

(** val coq_RzQ_to_standard : RzQGateSet.coq_RzQ_ucom_l -> standard_ucom_l **)

let coq_RzQ_to_standard l =
  change_gate_set coq_RzQ_to_standard_u l

(** val decompose_to_cnot_u :
    StandardGateSet.coq_Std_Unitary gate_app -> standard_ucom_l **)

let decompose_to_cnot_u g = match g with
| App1 (_, _) -> g :: []
| App2 (s, m, n) ->
  (match s with
   | StandardGateSet.U_CZ ->
     (App1 (StandardGateSet.U_H, n)) :: ((App2 (StandardGateSet.U_CX, m,
       n)) :: ((App1 (StandardGateSet.U_H, n)) :: []))
   | StandardGateSet.U_SWAP ->
     (App2 (StandardGateSet.U_CX, m, n)) :: ((App2 (StandardGateSet.U_CX, n,
       m)) :: ((App2 (StandardGateSet.U_CX, m, n)) :: []))
   | _ -> g :: [])
| App3 (s, m, n, p) ->
  (match s with
   | StandardGateSet.U_CCX ->
     (App1 (StandardGateSet.U_H, p)) :: ((App2 (StandardGateSet.U_CX, n,
       p)) :: ((App1 (StandardGateSet.U_Tdg, p)) :: ((App2
       (StandardGateSet.U_CX, m, p)) :: ((App1 (StandardGateSet.U_T,
       p)) :: ((App2 (StandardGateSet.U_CX, n, p)) :: ((App1
       (StandardGateSet.U_Tdg, p)) :: ((App2 (StandardGateSet.U_CX, m,
       p)) :: ((App2 (StandardGateSet.U_CX, m, n)) :: ((App1
       (StandardGateSet.U_Tdg, n)) :: ((App2 (StandardGateSet.U_CX, m,
       n)) :: ((App1 (StandardGateSet.U_T, m)) :: ((App1
       (StandardGateSet.U_T, n)) :: ((App1 (StandardGateSet.U_T,
       p)) :: ((App1 (StandardGateSet.U_H, p)) :: []))))))))))))))
   | StandardGateSet.U_CCZ ->
     (App2 (StandardGateSet.U_CX, n, p)) :: ((App1 (StandardGateSet.U_Tdg,
       p)) :: ((App2 (StandardGateSet.U_CX, m, p)) :: ((App1
       (StandardGateSet.U_T, p)) :: ((App2 (StandardGateSet.U_CX, n,
       p)) :: ((App1 (StandardGateSet.U_Tdg, p)) :: ((App2
       (StandardGateSet.U_CX, m, p)) :: ((App2 (StandardGateSet.U_CX, m,
       n)) :: ((App1 (StandardGateSet.U_Tdg, n)) :: ((App2
       (StandardGateSet.U_CX, m, n)) :: ((App1 (StandardGateSet.U_T,
       m)) :: ((App1 (StandardGateSet.U_T, n)) :: ((App1
       (StandardGateSet.U_T, p)) :: []))))))))))))
   | _ -> g :: [])

(** val decompose_to_cnot :
    standard_ucom_l -> StandardGateSet.coq_Std_Unitary gate_list **)

let decompose_to_cnot l =
  change_gate_set decompose_to_cnot_u l

(** val convert_to_ibm : standard_ucom_l -> standard_ucom_l **)

let convert_to_ibm l =
  coq_IBM_to_standard (standard_to_IBM l)

(** val convert_to_rzq : standard_ucom_l -> standard_ucom_l **)

let convert_to_rzq l =
  coq_RzQ_to_standard (standard_to_RzQ l)

(** val replace_rzq_u :
    StandardGateSet.coq_Std_Unitary gate_app -> standard_ucom_l **)

let replace_rzq_u g = match g with
| App1 (s, m) ->
  (match s with
   | StandardGateSet.U_Rzq q ->
     if Q.equal q (Q.of_int 0)
     then (App1 (StandardGateSet.U_I, m)) :: []
     else if Q.equal q (Q.of_int 1)
          then (App1 (StandardGateSet.U_Z, m)) :: []
          else if Q.equal q (Q.of_ints 1 2)
               then (App1 (StandardGateSet.U_S, m)) :: []
               else if Q.equal q (Q.of_ints 3 2)
                    then (App1 (StandardGateSet.U_Sdg, m)) :: []
                    else if Q.equal q (Q.of_ints 1 4)
                         then (App1 (StandardGateSet.U_T, m)) :: []
                         else if Q.equal q (Q.of_ints 7 4)
                              then (App1 (StandardGateSet.U_Tdg, m)) :: []
                              else (App1 ((StandardGateSet.U_Rz
                                     (( *. ) (Q.to_float q) Float.pi)),
                                     m)) :: []
   | _ -> g :: [])
| _ -> g :: []

(** val replace_rzq :
    standard_ucom_l -> StandardGateSet.coq_Std_Unitary gate_list **)

let replace_rzq l =
  change_gate_set replace_rzq_u l
