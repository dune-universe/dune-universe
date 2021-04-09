open CXCancellation
open ConnectivityGraph
open GateCancellation
open HadamardReduction
open Layouts
open NotPropagation
open Optimize1qGates
open RotationMerging
open SimpleMapping
open UnitaryListRepresentation

type circ = StandardGateSet.standard_ucom_l

type gate_counts =
| BuildCounts of int * int * int * int * int * int * int * int * int * 
   int * int * int * int * int * int * int * int * int * int * int * 
   int

type layout = qmap

type c_graph = (int * (int -> int -> int list)) * (int -> int -> bool)

(** val get_dim : c_graph -> int **)

let get_dim cg =
  fst (fst cg)

(** val get_get_path : c_graph -> int -> int -> int list **)

let get_get_path cg =
  snd (fst cg)

(** val get_is_in_graph : c_graph -> int -> int -> bool **)

let get_is_in_graph =
  snd

(** val check_well_typed : circ -> int -> bool **)

let check_well_typed c n =
  uc_well_typed_l_b n ((fun x _ -> x) c n)

(** val convert_to_ibm : circ -> StandardGateSet.standard_ucom_l **)

let convert_to_ibm =
  StandardGateSet.convert_to_ibm

(** val convert_to_rzq : circ -> StandardGateSet.standard_ucom_l **)

let convert_to_rzq =
  StandardGateSet.convert_to_rzq

(** val replace_rzq :
    circ -> StandardGateSet.StandardGateSet.coq_Std_Unitary gate_list **)

let replace_rzq =
  StandardGateSet.replace_rzq

(** val decompose_to_cnot :
    circ -> StandardGateSet.StandardGateSet.coq_Std_Unitary gate_list **)

let decompose_to_cnot =
  StandardGateSet.decompose_to_cnot

(** val count_I : circ -> int **)

let count_I l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_I -> true
         | _ -> false)
      | _ -> false) l)

(** val count_X : circ -> int **)

let count_X l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_X -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Y : circ -> int **)

let count_Y l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Y -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Z : circ -> int **)

let count_Z l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Z -> true
         | _ -> false)
      | _ -> false) l)

(** val count_H : circ -> int **)

let count_H l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_H -> true
         | _ -> false)
      | _ -> false) l)

(** val count_S : circ -> int **)

let count_S l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_S -> true
         | _ -> false)
      | _ -> false) l)

(** val count_T : circ -> int **)

let count_T l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_T -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Sdg : circ -> int **)

let count_Sdg l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Sdg -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Tdg : circ -> int **)

let count_Tdg l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Tdg -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Rx : circ -> int **)

let count_Rx l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Rx _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Ry : circ -> int **)

let count_Ry l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Ry _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Rz : circ -> int **)

let count_Rz l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Rz _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Rzq : circ -> int **)

let count_Rzq l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_Rzq _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_U1 : circ -> int **)

let count_U1 l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_U1 _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_U2 : circ -> int **)

let count_U2 l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_U2 (_, _) -> true
         | _ -> false)
      | _ -> false) l)

(** val count_U3 : circ -> int **)

let count_U3 l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_U3 (_, _, _) -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CX : circ -> int **)

let count_CX l =
  List.length
    (List.filter (fun g ->
      match g with
      | App2 (y, _, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_CX -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CZ : circ -> int **)

let count_CZ l =
  List.length
    (List.filter (fun g ->
      match g with
      | App2 (y, _, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_CZ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_SWAP : circ -> int **)

let count_SWAP l =
  List.length
    (List.filter (fun g ->
      match g with
      | App2 (y, _, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_SWAP -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CCX : circ -> int **)

let count_CCX l =
  List.length
    (List.filter (fun g ->
      match g with
      | App3 (y, _, _, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_CCX -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CCZ : circ -> int **)

let count_CCZ l =
  List.length
    (List.filter (fun g ->
      match g with
      | App3 (y, _, _, _) ->
        (match y with
         | StandardGateSet.StandardGateSet.U_CCZ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_gates : circ -> gate_counts **)

let count_gates l =
  BuildCounts ((count_I l), (count_X l), (count_Y l), (count_Z l),
    (count_H l), (count_S l), (count_T l), (count_Sdg l), (count_Tdg l),
    (count_Rx l), (count_Ry l), (count_Rz l), (count_Rzq l), (count_U1 l),
    (count_U2 l), (count_U3 l), (count_CX l), (count_CZ l), (count_SWAP l),
    (count_CCX l), (count_CCZ l))

(** val total_gate_count : circ -> int **)

let total_gate_count =
  List.length

(** val count_clifford_rzq : circ -> int **)

let count_clifford_rzq l =
  let f = fun g ->
    match g with
    | App1 (y, _) ->
      (match y with
       | StandardGateSet.StandardGateSet.U_Rzq q ->
         let q' = RzQGateSet.bound q in
         (||)
           ((||)
             ((||) (Q.equal q' (Q.of_int 0)) (Q.equal q' (Q.of_ints 1 2)))
             (Q.equal q' (Q.of_ints 3 2))) (Q.equal q' (Q.of_int 1))
       | _ -> false)
    | _ -> false
  in
  List.length (List.filter f l)

(** val scale_count : gate_counts -> int -> gate_counts **)

let scale_count gc n0 =
  let BuildCounts (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s,
                   t, u) = gc
  in
  BuildCounts ((( * ) n0 a), (( * ) n0 b), (( * ) n0 c), (( * ) n0 d),
  (( * ) n0 e), (( * ) n0 f), (( * ) n0 g), (( * ) n0 h), (( * ) n0 i),
  (( * ) n0 j), (( * ) n0 k), (( * ) n0 l), (( * ) n0 m), (( * ) n0 n),
  (( * ) n0 o), (( * ) n0 p), (( * ) n0 q), (( * ) n0 r), (( * ) n0 s),
  (( * ) n0 t), (( * ) n0 u))

(** val add_counts : gate_counts -> gate_counts -> gate_counts **)

let add_counts gc gc' =
  let BuildCounts (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s,
                   t, u) = gc
  in
  let BuildCounts (a', b', c', d', e', f', g', h', i', j', k', l', m', n',
                   o', p', q', r', s', t', u') = gc'
  in
  BuildCounts (((+) a a'), ((+) b b'), ((+) c c'), ((+) d d'), ((+) e e'),
  ((+) f f'), ((+) g g'), ((+) h h'), ((+) i i'), ((+) j j'), ((+) k k'),
  ((+) l l'), ((+) m m'), ((+) n n'), ((+) o o'), ((+) p p'), ((+) q q'),
  ((+) r r'), ((+) s s'), ((+) t t'), ((+) u u'))

(** val count_gates_lcr : ((circ * circ) * circ) -> int -> gate_counts **)

let count_gates_lcr lcr n =
  let (lc, r) = lcr in
  let (l, c) = lc in
  let ln = count_gates l in
  let cn = count_gates c in
  let rn = count_gates r in
  add_counts
    (add_counts ln
      (scale_count cn ((-) n (Pervasives.succ (Pervasives.succ 0))))) rn

(** val optimize_1q_gates : circ -> circ **)

let optimize_1q_gates c =
  StandardGateSet.coq_IBM_to_standard
    (optimize_1q_gates (StandardGateSet.standard_to_IBM c))

(** val cx_cancellation : circ -> circ **)

let cx_cancellation c =
  StandardGateSet.coq_IBM_to_standard
    (cx_cancellation (StandardGateSet.standard_to_IBM c))

(** val optimize_ibm : circ -> circ **)

let optimize_ibm c =
  StandardGateSet.coq_IBM_to_standard
    (CXCancellation.cx_cancellation
      (Optimize1qGates.optimize_1q_gates (StandardGateSet.standard_to_IBM c)))

(** val not_propagation : circ -> circ **)

let not_propagation c =
  StandardGateSet.coq_RzQ_to_standard
    (not_propagation (StandardGateSet.standard_to_RzQ c))

(** val hadamard_reduction : circ -> circ **)

let hadamard_reduction c =
  StandardGateSet.coq_RzQ_to_standard
    (hadamard_reduction (StandardGateSet.standard_to_RzQ c))

(** val cancel_single_qubit_gates : circ -> circ **)

let cancel_single_qubit_gates c =
  StandardGateSet.coq_RzQ_to_standard
    (cancel_single_qubit_gates (StandardGateSet.standard_to_RzQ c))

(** val cancel_two_qubit_gates : circ -> circ **)

let cancel_two_qubit_gates c =
  StandardGateSet.coq_RzQ_to_standard
    (cancel_two_qubit_gates (StandardGateSet.standard_to_RzQ c))

(** val merge_rotations : circ -> circ **)

let merge_rotations c =
  StandardGateSet.coq_RzQ_to_standard
    (merge_rotations (StandardGateSet.standard_to_RzQ c))

(** val optimize_nam : circ -> circ **)

let optimize_nam c =
  StandardGateSet.coq_RzQ_to_standard
    (GateCancellation.cancel_single_qubit_gates
      (GateCancellation.cancel_two_qubit_gates
        (RotationMerging.merge_rotations
          (GateCancellation.cancel_single_qubit_gates
            (HadamardReduction.hadamard_reduction
              (GateCancellation.cancel_two_qubit_gates
                (GateCancellation.cancel_single_qubit_gates
                  (GateCancellation.cancel_two_qubit_gates
                    (HadamardReduction.hadamard_reduction
                      (NotPropagation.not_propagation
                        (StandardGateSet.standard_to_RzQ c)))))))))))

(** val optimize_nam_light : circ -> circ **)

let optimize_nam_light c =
  StandardGateSet.coq_RzQ_to_standard
    (GateCancellation.cancel_single_qubit_gates
      (HadamardReduction.hadamard_reduction
        (GateCancellation.cancel_two_qubit_gates
          (GateCancellation.cancel_single_qubit_gates
            (GateCancellation.cancel_two_qubit_gates
              (HadamardReduction.hadamard_reduction
                (NotPropagation.not_propagation
                  (StandardGateSet.standard_to_RzQ c))))))))

(** val optimize_nam_lcr : circ -> ((circ * circ) * circ) option **)

let optimize_nam_lcr c =
  coq_LCR c optimize_nam StandardGateSet.StandardGateSet.match_gate

(** val check_layout : layout -> int -> bool **)

let check_layout la n =
  layout_well_formed_b n ((fun x _ -> x) la n)

(** val check_graph : c_graph -> bool **)

let check_graph cg =
  let n = get_dim cg in
  let get_path0 = get_get_path cg in
  let is_in_graph0 = get_is_in_graph cg in
  check_graph n get_path0 is_in_graph0

(** val check_constraints : circ -> c_graph -> bool **)

let check_constraints c cg =
  let n = get_dim cg in
  let is_in_graph0 = get_is_in_graph cg in
  respects_constraints_directed_b is_in_graph0 ((fun x _ -> x) c n)

(** val simple_map :
    circ -> layout -> c_graph -> StandardGateSet.standard_ucom_l * qmap **)

let simple_map c la cg =
  let n = get_dim cg in
  let get_path0 = get_get_path cg in
  let is_in_graph0 = get_is_in_graph cg in
  simple_map ((fun x _ -> x) c n) ((fun x _ -> x) la n) get_path0 is_in_graph0

(** val make_tenerife : unit -> c_graph **)

let make_tenerife _ =
  (((Pervasives.succ (Pervasives.succ (Pervasives.succ (Pervasives.succ
    (Pervasives.succ 0))))), Tenerife.get_path), Tenerife.is_in_graph)

(** val make_lnn : int -> c_graph **)

let make_lnn n =
  ((n, LNN.get_path), (LNN.is_in_graph n))

(** val make_lnn_ring : int -> c_graph **)

let make_lnn_ring n =
  ((n, (LNNRing.get_path n)), (LNNRing.is_in_graph n))

(** val make_grid : int -> int -> c_graph **)

let make_grid m n =
  (((( * ) m n), (Grid.get_path n)), (Grid.is_in_graph m n))

(** val trivial_layout : int -> layout **)

let trivial_layout =
  trivial_layout

(** val list_to_layout : int list -> layout **)

let list_to_layout =
  list_to_layout

(** val layout_to_list : layout -> int -> int list **)

let layout_to_list la n =
  layout_to_list n ((fun x _ -> x) la n)
