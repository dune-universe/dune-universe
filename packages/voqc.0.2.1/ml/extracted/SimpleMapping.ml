open Layouts
open UnitaryListRepresentation

(** val respects_constraints_directed_b :
    (int -> int -> bool) -> StandardGateSet.standard_ucom_l -> bool **)

let rec respects_constraints_directed_b is_in_graph = function
| [] -> true
| g :: t ->
  (match g with
   | App1 (_, _) -> respects_constraints_directed_b is_in_graph t
   | App2 (s, n1, n2) ->
     (match s with
      | StandardGateSet.StandardGateSet.U_CX ->
        (&&) (is_in_graph n1 n2)
          (respects_constraints_directed_b is_in_graph t)
      | _ -> false)
   | App3 (_, _, _, _) -> false)

(** val decompose_swaps_u :
    (int -> int -> bool) -> StandardGateSet.StandardGateSet.coq_Std_Unitary
    gate_app -> StandardGateSet.standard_ucom_l **)

let decompose_swaps_u is_in_graph g = match g with
| App2 (s, m, n) ->
  (match s with
   | StandardGateSet.StandardGateSet.U_SWAP ->
     if is_in_graph m n
     then (App2 (StandardGateSet.StandardGateSet.U_CX, m, n)) :: ((App2
            (StandardGateSet.StandardGateSet.U_CX, n, m)) :: ((App2
            (StandardGateSet.StandardGateSet.U_CX, m, n)) :: []))
     else (App2 (StandardGateSet.StandardGateSet.U_CX, n, m)) :: ((App2
            (StandardGateSet.StandardGateSet.U_CX, m, n)) :: ((App2
            (StandardGateSet.StandardGateSet.U_CX, n, m)) :: []))
   | _ -> g :: [])
| _ -> g :: []

(** val decompose_swaps :
    (int -> int -> bool) -> StandardGateSet.standard_ucom_l ->
    StandardGateSet.StandardGateSet.coq_Std_Unitary gate_list **)

let decompose_swaps is_in_graph l =
  StandardGateSet.change_gate_set (decompose_swaps_u is_in_graph) l

(** val coq_SWAP :
    int -> int -> StandardGateSet.StandardGateSet.coq_Std_Unitary gate_app **)

let coq_SWAP a b =
  App2 (StandardGateSet.StandardGateSet.U_SWAP, a, b)

(** val coq_CNOT :
    int -> int -> StandardGateSet.StandardGateSet.coq_Std_Unitary gate_app **)

let coq_CNOT a b =
  App2 (StandardGateSet.StandardGateSet.U_CX, a, b)

(** val coq_H :
    int -> StandardGateSet.StandardGateSet.coq_Std_Unitary gate_app **)

let coq_H a =
  App1 (StandardGateSet.StandardGateSet.U_H, a)

(** val path_to_swaps :
    int list -> qmap -> StandardGateSet.standard_ucom_l * qmap **)

let rec path_to_swaps p m =
  match p with
  | [] -> ([], m)
  | n1 :: t ->
    (match t with
     | [] -> ([], m)
     | n2 :: l ->
       (match l with
        | [] -> ([], m)
        | _ :: _ ->
          let (l0, m') = path_to_swaps t (swap_in_map m n1 n2) in
          (((coq_SWAP n1 n2) :: l0), m')))

(** val fix_cnots :
    StandardGateSet.standard_ucom_l -> (int -> int -> bool) ->
    StandardGateSet.standard_ucom_l **)

let rec fix_cnots l is_in_graph =
  match l with
  | [] -> l
  | h :: t ->
    (match h with
     | App2 (s, n1, n2) ->
       (match s with
        | StandardGateSet.StandardGateSet.U_CX ->
          if is_in_graph n1 n2
          then (coq_CNOT n1 n2) :: (fix_cnots t is_in_graph)
          else (coq_H n1) :: ((coq_H n2) :: ((coq_CNOT n2 n1) :: ((coq_H n1) :: (
                 (coq_H n2) :: (fix_cnots t is_in_graph)))))
        | _ -> h :: (fix_cnots t is_in_graph))
     | _ -> h :: (fix_cnots t is_in_graph))

(** val insert_swaps :
    StandardGateSet.standard_ucom_l -> qmap -> (int -> int -> int list) ->
    StandardGateSet.standard_ucom_l * qmap **)

let rec insert_swaps l m get_path =
  match l with
  | [] -> ([], m)
  | g :: t ->
    (match g with
     | App1 (u, n) ->
       let (t', m') = insert_swaps t m get_path in
       (((App1 (u, (log2phys m n))) :: t'), m')
     | App2 (s, n1, n2) ->
       (match s with
        | StandardGateSet.StandardGateSet.U_CX ->
          let p = get_path (log2phys m n1) (log2phys m n2) in
          let (swaps, m') = path_to_swaps p m in
          let mapped_cnot =
            List.append swaps
              ((coq_CNOT (log2phys m' n1) (log2phys m' n2)) :: [])
          in
          let (t', m'') = insert_swaps t m' get_path in
          ((List.append mapped_cnot t'), m'')
        | _ -> ([], m))
     | App3 (_, _, _, _) -> ([], m))

(** val simple_map :
    StandardGateSet.standard_ucom_l -> qmap -> (int -> int -> int list) ->
    (int -> int -> bool) -> StandardGateSet.standard_ucom_l * qmap **)

let simple_map l m get_path is_in_graph =
  let l' = StandardGateSet.decompose_to_cnot l in
  let (lm, m') = insert_swaps l' m get_path in
  let lm' = decompose_swaps is_in_graph lm in
  let lfin = fix_cnots lm' is_in_graph in (lfin, m')
