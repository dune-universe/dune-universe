open UnitaryListRepresentation

(** val apply_H_equivalence1 :
    int -> RzQGateSet.coq_RzQ_ucom_l -> RzQGateSet.RzQGateSet.coq_RzQ_Unitary
    gate_list option **)

let apply_H_equivalence1 q l =
  RzQGateSet.replace_pattern l
    ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_P q) :: ((RzQGateSet.coq_H q) :: [])))
    ((RzQGateSet.coq_PDAG q) :: ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_PDAG
                                                            q) :: [])))

(** val apply_H_equivalence2 :
    int -> RzQGateSet.coq_RzQ_ucom_l -> RzQGateSet.RzQGateSet.coq_RzQ_Unitary
    gate_list option **)

let apply_H_equivalence2 q l =
  RzQGateSet.replace_pattern l
    ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_PDAG q) :: ((RzQGateSet.coq_H q) :: [])))
    ((RzQGateSet.coq_P q) :: ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_P q) :: [])))

(** val apply_H_equivalence3 :
    int -> RzQGateSet.coq_RzQ_ucom_l -> RzQGateSet.RzQGateSet.coq_RzQ_Unitary
    gate_app list option **)

let apply_H_equivalence3 q l =
  match RzQGateSet.remove_prefix l
          ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_P q) :: [])) with
  | Some l1 ->
    (match next_two_qubit_gate l1 q with
     | Some p ->
       let (p0, l3) = p in
       let (p1, q2) = p0 in
       let (p2, q1) = p1 in
       let (l2, r) = p2 in
       (match r with
        | RzQGateSet.RzQGateSet.URzQ_CNOT ->
          if (=) q q2
          then (match RzQGateSet.remove_prefix l3
                        ((RzQGateSet.coq_PDAG q) :: ((RzQGateSet.coq_H q) :: [])) with
                | Some l4 ->
                  Some
                    (List.append l2
                      (List.append
                        ((RzQGateSet.coq_PDAG q2) :: ((RzQGateSet.coq_CNOT q1
                                                        q2) :: ((RzQGateSet.coq_P
                                                                  q2) :: [])))
                        l4))
                | None -> None)
          else None
        | _ -> None)
     | None -> None)
  | None -> None

(** val apply_H_equivalence4 :
    int -> RzQGateSet.coq_RzQ_ucom_l -> RzQGateSet.RzQGateSet.coq_RzQ_Unitary
    gate_app list option **)

let apply_H_equivalence4 q l =
  match RzQGateSet.remove_prefix l
          ((RzQGateSet.coq_H q) :: ((RzQGateSet.coq_PDAG q) :: [])) with
  | Some l1 ->
    (match next_two_qubit_gate l1 q with
     | Some p ->
       let (p0, l3) = p in
       let (p1, q2) = p0 in
       let (p2, q1) = p1 in
       let (l2, r) = p2 in
       (match r with
        | RzQGateSet.RzQGateSet.URzQ_CNOT ->
          if (=) q q2
          then (match RzQGateSet.remove_prefix l3
                        ((RzQGateSet.coq_P q) :: ((RzQGateSet.coq_H q) :: [])) with
                | Some l4 ->
                  Some
                    (List.append l2
                      (List.append
                        ((RzQGateSet.coq_P q2) :: ((RzQGateSet.coq_CNOT q1 q2) :: (
                        (RzQGateSet.coq_PDAG q2) :: []))) l4))
                | None -> None)
          else None
        | _ -> None)
     | None -> None)
  | None -> None

(** val coq_H_equivalences :
    int -> (RzQGateSet.coq_RzQ_ucom_l -> RzQGateSet.coq_RzQ_ucom_l option)
    list **)

let coq_H_equivalences q =
  (apply_H_equivalence1 q) :: ((apply_H_equivalence2 q) :: ((apply_H_equivalence3
                                                              q) :: (
    (apply_H_equivalence4 q) :: [])))

(** val apply_H_equivalences' :
    RzQGateSet.coq_RzQ_ucom_l -> (int -> (RzQGateSet.coq_RzQ_ucom_l ->
    RzQGateSet.coq_RzQ_ucom_l option) list) -> int ->
    RzQGateSet.RzQGateSet.coq_RzQ_Unitary gate_app list ->
    RzQGateSet.coq_RzQ_ucom_l **)

let rec apply_H_equivalences' l equivs n acc =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> List.rev_append acc l)
    (fun n' ->
    match l with
    | [] -> List.rev_append acc []
    | g :: t ->
      (match g with
       | App1 (r, q) ->
         (match r with
          | RzQGateSet.RzQGateSet.URzQ_H ->
            (match try_rewrites l (equivs q) with
             | Some l' -> apply_H_equivalences' l' equivs n' acc
             | None ->
               apply_H_equivalences' t equivs n' ((RzQGateSet.coq_H q) :: acc))
          | _ -> apply_H_equivalences' t equivs n' (g :: acc))
       | _ -> apply_H_equivalences' t equivs n' (g :: acc)))
    n

(** val hadamard_reduction :
    RzQGateSet.coq_RzQ_ucom_l -> RzQGateSet.coq_RzQ_ucom_l **)

let hadamard_reduction l =
  apply_H_equivalences' l coq_H_equivalences
    (( * ) (Pervasives.succ (Pervasives.succ 0)) (List.length l)) []
