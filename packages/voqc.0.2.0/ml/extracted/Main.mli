open StandardGateSet
open UnitaryListRepresentation

(** {1 Overview} *)

(** VOQC contains utilities for optimizing quantum circuits and mapping them
   to a connectivity graph. VOQC was first presented at POPL 2021. The extended
   version of the paper is available {{:https://arxiv.org/abs/1912.02250}here}.
   In the remainder of this document we will refer to this paper as "POPL VOQC". *)

(** This file lists the functions we provide in VOQC, as well as properties we 
   have verified about them. In general, our goal is to prove that VOQC circuit 
   transformations are {i semantics preserving }, meaning that the behavior of a 
   circuit (its "semantics") does not change after our transformations are applied. 
   The semantics of a unitary circuit over d qubits is a 2^d x 2^d 
   unitary matrix, so proving semantics-preservation requires showing that two 
   (symbolic) matrices are equal. Sometimes we also show that the output of a 
   transformation has a particular property, such as using a particular gate set. 
   
   In the remainder of this document, we will use the following shorthand for 
   properties proved:
- {i Preserves semantics } -- The semantics-preservation property is written in Coq as 
   [ forall (c : circ), [[opt c]] = [[c]]] for optimization [ opt]. In our development, 
   we support several variations on semantics-preservation:
{ul {- {i (WT) } -- We include the annotation (WT) to indicate that a transformation 
   preserves semantics provided that the input circuit is well-typed. A circuit is 
   "well-typed" for some dimension (i.e. number of qubits) d if every gate in the 
   circuit applies to some qubit less than d and no gate has duplicate arguments. 
   Some optimizations do not preserve semantics when the input is ill-typed because 
   they can produce a well-typed circuit from an ill-typed input. In Coq, this 
   requirement is written as [ forall c, well_typed c -> [[opt c]] = [[c]]].}
{- {i (phase) } -- We include the annotation (phase) to indicate that the semantics 
   are preserved up to a global phase. The unitary matrices X and e^(ix)X have the 
   same action on all input states, and can thus be considered equivalent. However, 
   this definition of equivalence is not compositional in the sense that the controlled 
   versions of these matrices are not equivalent (because the phase is no longer global). 
   In Coq, this restriction is written as [ forall c, exists x, [[opt c]] = e^(ix) [[c]]].}
{- {i (perm) } -- We include the annotation (perm) to indicate that the semantics are preserved 
   up to a permutation of qubits. This will be the case after applying circuit mapping. 
   In Coq, for permutation matrices P1 and P2, this variation is written as  
   [ forall c, [[c]] = P1 x [[opt c]] x P2].}}
- {i Preserves WT} -- This property says that if the input circuit is well-typed, 
   then the output circuit will also be well-typed. In our semantics, the denotation of 
   a well-typed circuit is a unitary matrix while the denotation of an ill-typed circuit 
   is the zero matrix, so all of the variations on semantics-preservation actually 
   imply WT-preservation.
- {i Preserves mapping} -- This property says that, for any connectivity graph G, if the 
   input circuit respects the constraints in G then the output 
   circuit also respects the constraints in G. This property allows us to apply 
   optimizations after mapping, preserving our guarantee that the output program 
   satisfies hardware constraints.
- {i Uses gates \{g1, ..., gn\}} -- This property says that every operation in the 
   circuit is an application of one of \{g1, ..., gn\}.
- {i Respects constraints G} -- This property says that every two-qubit interaction 
   in the circuit is allowed by the connectivity graph G.

*)

(** {1 Types} *)

(** The [circ] type is a list of gates from the standard gate set. *)
type circ = StandardGateSet.coq_Std_Unitary gate_list

(** [gate_counts] is the return type for [count_gates]. It stores the count of
   each gate type in the following order: \{ I, X, Y, Z, H, S, T, Sdg, Tdg, Rx, 
   Ry, Rz, Rz, U1, U2, U3, CX, CZ , SWAP, CCX, CCZ \}. *)
type gate_counts =
| BuildCounts of int * int * int * int * int * int * int * int * int * 
   int * int * int * int * int * int * int * int * int * int * int * 
   int

(** A {i layout} describes a mapping from logical to physical qubits.
   For an architecture with n qubits, a layout should be a bijection on 
   integers \{0, ..., n-1\}. Under the hood, [layout] is a pair of functions 
   [(nat -> nat) * (nat -> nat)] that maps logical qubits to physical qubits 
   and physical qubits back to logical qubits. A layout is {i well-formed} if 
   these two functions accurately represent a bijection and its inverse.
   
   You can construct a layout using the function [list_to_layout]. *)
type layout

(** A {i connectivity graph} ([c_graph]) is a triple [ nat * (nat -> nat -> list nat) * (nat -> nat -> bool)], 
   which consists of the total number of qubits in the system, a function to generate an 
   undirected path between any two qubits, and an oracle that indicates whether a directed 
   edge exists between two qubits. A connectivity graph is {i well-formed} if the 
   function to get paths returns a valid path between any two qubits in the system. 
   
   You can construct a connectivity graph using [make_tenerife], [make_lnn], [make_lnn_ring],
   or [make_grid]. *)
type c_graph

(** {1 API} *)

(** {2 Utility Functions} *)

(** Check if a circuit is well-typed with the given number of qubits (i.e. every 
   gate is applied to qubits within range and never applied to duplicates). *)
val check_well_typed : circ -> int -> bool

(** Restrict a program to use gates in the IBM gate set (this may be done implicitly 
   when calling certain optimizations).

   {i Verified Properties }: preserves semantics, preserves WT, preserves mapping, 
   uses gates \{U1, U2, U3, CX\} *)
val convert_to_ibm : circ -> circ

(** Restrict a circuit to use gates in the RzQ gate set (this may be done implicitly 
   when calling certain optimizations).
    
   {i Verified properties:} preserves semantics (phase), preserves WT, 
   preserves mapping, uses gates \{H, X, Rzq, CX\} *)
val convert_to_rzq : circ -> circ

(** Replace the (non-standard) Rzq gate with its equivalent Rz, Z, T, S, Tdg, or Sdg gate.
    
   {i Verified Properties:} preserves semantics, preserves WT, preserves mapping, 
   uses any gate in the standard set except Rzq *)
val replace_rzq : circ -> circ

(** Decompose CZ, SWAP, CCX, and CCZ gates so that the only multi-qubit gate is 
   CX (also called "CNOT").
    
   {i Verified Properties:} preserves semantics, preserves WT, uses any gate in 
   the standard set except \{CZ, SWAP, CCX, CCZ\} *)
val decompose_to_cnot : circ -> circ

(** Count all types of gates in the program. Note that this count is {i syntactic} 
   in the sense that gates like Rz, Rzq, and U1 are counted separately even 
   though they all could be considered a U1 gate. Similarly, the count for X does
   not include gates of the form U3(PI,0,PI), even though this is functionally the same. *)
val count_gates : circ -> gate_counts

(** Count the total number of gates (i.e. length of the instruction list). *)
val total_gate_count : circ -> int

(** Count the Rzq gates parameterized by a multiple of PI/2 (i.e. "Clifford" Rz gates). *)
val count_clifford_rzq : circ -> int

(** Multiply all gate counts by a factor n. *)
val scale_count : gate_counts -> int -> gate_counts

(** Add gate counts. *)
val add_counts : gate_counts -> gate_counts -> gate_counts

(** Compute the gates required for n iterations of the input LCR decomposition 
   (see [optimize_nam_lcr]).  *)
val count_gates_lcr : ((circ * circ) * circ) -> int -> gate_counts

(** {2 Optimization Functions} *)

(** Implementation of Qiskit's {{:https://qiskit.org/documentation/stubs/qiskit.transpiler.passes.Optimize1qGates.html}Optimize1qGates} 
   routine, which merges adjacent 1-qubit gates. Internally uses the IBM gate set. 
   
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val optimize_1q_gates : circ -> circ

(** Implementation of Qiskit's {{:https://qiskit.org/documentation/stubs/qiskit.transpiler.passes.CXCancellation.html}CXCancellation} 
   routine,  which cancels adjacent CX gates. Internally uses the IBM gate set. 
   
   {i Verified Properties:} Preserves semantics (WT), preserves WT, preserves mapping *)
val cx_cancellation : circ -> circ

(** Run [optimize_1q_gates] followed by [cx_cancellation].
   
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val optimize_ibm : circ -> circ

(** Implementation of Nam et al.'s "NOT propagation," which commutes X gates rightward 
    through the circuit, cancelling them when possible (see VOQC POPL Sec 4.3). 
    Internally uses the RzQ gate set.
  
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val not_propagation : circ -> circ 

(** Implementation of Nam et al.'s "Hadamard gate reduction," which applies a series 
    of identities to reduce the number of H gates (see VOQC POPL Sec 4.4). 
    Internally uses the RzQ gate set.
  
   {i Verified Properties:} Preserves semantics (phase), preserves WT, preserves mapping *)
val hadamard_reduction : circ -> circ

(** Implementation of Nam et al.'s "single-qubit gate cancellation," which commutes 
   single-qubit gates rightward through the circuit, cancelling them when possible, 
   and reverting them to their original positions if they fail to cancel (see VOQC 
   POPL Sec 4.3). Internally uses the RzQ gate set.
  
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val cancel_single_qubit_gates : circ -> circ

(** Implementation of Nam et al.'s "two-qubit gate cancellation," which commutes 
   CX gates rightward through the circuit, cancelling them when possible, 
   and reverting them to their original positions if they fail to cancel (see VOQC 
   POPL Sec 4.3). Internally uses the RzQ gate set.
  
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val cancel_two_qubit_gates : circ -> circ 

(** Implementation of Nam et al.'s "rotation merging using phase polynomials," 
   which combines Rz gates that act on the same logical state (see VOQC POPL
   Sec 4.4). Internally uses the RzQ gate set.
  
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val merge_rotations : circ -> circ

(** Run optimizations in the order 0, 1, 3, 2, 3, 1, 2, 4, 3, 2 where 0 is [not_propagation], 
   1 [hadamard_reduction], 2 is [cancel_single_qubit_gates], 3 is [cancel_two_qubit_gates], 
   and 4 is [merge_rotations] (see VOQC POPL Sec 4.6).
  
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val optimize_nam : circ -> circ

(** Run optimizations in the order 0, 1, 3, 2, 3, 1, 2, using the same numbering 
   scheme as above. This will be faster than [optimize_nam] because it excludes 
   rotation merging, which is our slowest optimization.
    
   {i Verified Properties:} Preserves semantics (WT, phase), preserves WT, preserves mapping *)
val optimize_nam_light : circ -> circ

(** Use [optimize_nam] to optimize across loop iterations. To count the gates 
   required for n iterations, use [count_gates_lcr].
    
    {i Verified Properties:} Say that this function returns [Some (l, c, r)] on 
    input [c0]. Then for any n >= 3, iterating [c0] n times is equivalent to 
    running [l] once, [c] n-2 times, and [r] once. If [c0] is well-typed then 
    [l, c, r] are also well-typed. If [c0] is properly mapped then [l, c, r] 
    are also properly mapped. *)
val optimize_nam_lcr : circ -> ((circ * circ) * circ) option

(** {2 Mapping Functions} *)

(** Check if a layout is well-formed. *)
val check_layout : layout -> int -> bool

(** Check if a graph is well-formed. This function will make O(n^2) calls to 
   the path finding function for n qubits, so it will likely be slow. *)
val check_graph : c_graph -> bool

(** Check if a circuit satisfies the constraints of a connectivity graph. *)
val check_constraints : circ -> c_graph -> bool

(** Return [(c, la)] where [c] is the mapped circuit and [la] is the final layout.
    
    {i Verified properties:} Provided that that the input circuit is well-typed 
    using the dimension stored in the input graph and the input layout and graph 
    are well-formed, this transformation preserves semantics (WT, perm) and preserves WT. 
    Furthermore, the output [c] respects the constraints the input graph and [la] 
    is well-formed. *)
val simple_map : circ -> layout -> c_graph -> circ * layout

(** Create a graph for {{:https://github.com/Qiskit/ibmq-device-information/blob/master/backends/tenerife/V1/version_log.md}IBM's 5-qubit Tenerife machine}).
    
    {i Verified Properties:} The output connectivity graph is well-formed. *)
val make_tenerife : unit -> c_graph

(** Create a 1D LNN graph with n qubits (see POPL VOQC Fig 8(b)).
    
    {i Verified Properties:} The output connectivity graph is well-formed. *)
val make_lnn : int -> c_graph

(** Create a 1D LNN ring graph with n qubits (see POPL VOQC Fig 8(c)).
    
    {i Verified Properties:} The output connectivity graph is well-formed. *)
val make_lnn_ring : int -> c_graph

(** Create a m x n 2D grid (see POPL VOQC Fig 8(d)).
    
    {i Verified Properties:} The output connectivity graph is well-formed. *)
val make_grid : int -> int -> c_graph

(** Create a trivial layout on n qubits (i.e. logical qubit i is mapped to physical qubit i).
    
    {i Verified Properties:} The output layout is well-formed.*)
val trivial_layout : int -> layout

(** Make a layout from a list. Example: the list [[3; 4; 1; 2; 0]] is transformed 
   to a layout with physical to logical qubit mapping \{0->3, 1->4, 2->1, 3->2, 4->0\}
   (so physical qubit 0 stores logical qubit 3) and the appropriate inverse logical 
   to physical mapping.*)
val list_to_layout : int list -> layout

(** Convert a layout to a list for easier printing. Example: the layout with 
   physical to logical qubit mapping \{0->3, 1->4, 2->1, 3->2, 4->0\} is
   transformed to the list [[3; 4; 1; 2; 0]].*)    
val layout_to_list : layout -> int -> int list
