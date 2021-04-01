# VOQC API

This file lists the functions we provide in VOQC, as well as properties we have verified about them. The VOQC executable in the top-level directory is a simple wraper around these functions.

## Verified Properties

In general, our goal is to prove that VOQC circuit transformations are *semantics preserving*, meaning that the behavior of a circuit (its "semantics") does not change after our transformations are applied. The semantics of a unitary circuit over d qubits is a 2^d x 2^d unitary matrix, so proving semantics-preservation requires showing that two (symbolic) matrices are equal. Sometimes we also show that the output of a transformation has a particular property, such as using a particular gate set. In the remainder of this document, we will use the following shorthand for different properties proved:
* **Preserves semantics** This is semantics-preservation, as described above. In Coq, this property is written as `forall (c : circ), [[opt c]] = [[c]]` for optimization `opt`.
* **Preserves semantics (WT)** We include the annotation (WT) to indicate that a transformation preserves semantics *provided* that the input circuit is well-typed. A circuit is "well-typed" for some dimension $d$ if every gate in the circuit applies to some qubit less than $d$ and no gate has duplicate arguments. Some optimizations do not preserve semantics when the input is ill-typed because they can produce a well-typed circuit from an ill-typed input. In Coq, this requirement is written as `forall c, well_typed c -> [[opt c]] = [[c]]`.
* **Preserves semantics (phase)** We include the annotation (phase) to indicate that the semantics are equal *up to a global phase*. The unitary matrices X and e^{ix}X have the same action on all input states, and can thus be considered equivalent. However, this definition of equivalence is not compositional in the sense that the controlled versions of these matrices are *not* equivalent (because the phase is no longer global). In Coq, this restriction is written as `forall c, exists $\theta$, [[opt c]] = $e^{i\theta}$ [[c]]`. The (phase) and (WT) annotations may appear together.
* **Preserves semantics (perm)** We include the annotation (perm) to indicate that the semantics are equal *up to a permutation of qubits*. This is used for talking about equivalence of circuits before and after mapping.
* **Preserves WT** This property says that if the input circuit is well-typed, then the output circuit will also be well-typed. In our semantics, the denotation of a well-typed circuit is a unitary matrix while the denotation of an ill-typed circuit is the zero matrix, so all of the variations on semantics-preservation actually imply WT-preservation. However, we list it as a separate property to avoid confusion.
* **Uses gates {g1, ..., gn}** Every operation in the circuit is an application of one of {g1, ..., gn}.
* **Respects constraints G** Every two-qubit interaction in the circuit is allowed by the connectivity graph G.
* **Preserves mapping** For all G, if the input circuit respects the constraints in G, then the output circuit also respects the constraints in G.

## Notation

In the API below, we will write function types as `f : T1 -> ... -> Tn`, which says that function `f` takes inputs of type `T1 -> ...`` and produces an output of type `Tn`. We will sometimes write `(x : T)` for a parameter type of type `T`, which gives a name `x` to that parameter that we can use in subsequent text. 
* `string`, `bool`, `int`, and `unit` are standard String, Boolean, Integer, and Unit types.
* `T option` is either `None` or `Some` applied to a value of type `T`.
* `T list` is a list with elements of type `T`.
* `T1 * ... * Tn` is a tuple with elements of type `T1 ... Tn`.
* `circ` is our type for quantum circuits.
* `gate_counts` is a product type describing the counts of different types of gates.
* `layout` and `c_graph` are types for describing the physical to logical qubit mapping and connectivity graph used in circuit mapping.

## List of Functions

### File I/O

* `read_qasm : string -> (circ * int)` 
   **Description:** Read an OpenQASM file and return a circuit along with the number of qubits it uses.
   **Verified properties:** N/A
* `write_qasm : circ -> (n : int) -> string -> unit` 
   **Description:** Write a circuit that uses n qubits to an OpenQASM file.
   **Verified properties:** N/A

### Utility

* `check_well_typed : circ -> (n : int) -> bool` 
  **Description:** Check if a circuit is well-typed with *n*-qubits (i.e. every gate is applied to qubits [0..*n*) and never applied to duplicates). 
  **Verified Properties:** This function returns true iff the circuit satisfies our Coq predicate for well-typedness.
* `convert_to_ibm : circ -> circ` 
  **Description:** Restrict a program to use gates in the IBM gate set (this may be done implicitly when calling certain optimizations). 
  **Verified properties:** preserves semantics, preserves WT, preserves mapping, uses gates {U1, U2, U3, CX}
* `convert_to_rzq : circ -> circ` 
  **Description:** Restrict a program to use gates in the RzQ gate set (this may be done implicitly when calling certain optimizations). 
  **Verified properties:** preserves semantics (phase), preserves WT, preserves mapping, uses gates {H, X, Rzq, CX}
* `replace_rzq : circ -> circ` 
  **Description:** Replace the (non-standard) Rzq gate with its equivalent Rz, Z, T, S, Tdg, or Sdg gate. 
  **Verified Properties:** preserves semantics, preserves WT, preserves mapping, uses any gate in the standard set except Rzq
* `decompose_to_cnot : circ -> circ` 
  **Description:** Decompose CZ, SWAP, CCX, and CCZ gates so that the only multi-qubit gate is CX (also called "CNOT"). 
  **Verified Properties:** preserves semantics, preserves WT, uses any gate in the standard set except {CZ, SWAP, CCX, CCZ}
* `count_<G> : circ -> int` 
  **Description:** Count each gate of type `<G>` (e.g. `count_H`, `count_X`). 
  **Verified Properties:** N/A
* `count_gates : circ -> gate_counts` 
  **Description:** Count all gate types. `gate_counts` is a product type storing the count for all gate types in the following order: { I, X, Y, Z, H, S, T, Sdg, Tdg, Rx, Ry, Rz, Rz, U1, U2, U3, CX, CZ , SWAP, CCX, CCZ }. 
  **Verified Properties:** N/A
* `total_gate_count : circ -> int`  
  **Description:** Get the total number of gates. 
  **Verified Properties:** The total count is equal to the sum of all counts returned by `count_gates`.
* `count_clifford_rzq : circ -> int` 
  **Description:** Count the Rzq gates parameterized by a multiple of PI/2 (i.e. "Clifford" Rz gates). 
  **Verified Properties:** N/A
* `scale_count : gate_counts -> (n : int) -> gate_counts` 
  **Description:** Multiply all gate counts by a factor *n*. 
  **Verified Properties:** For any circuit `c`, `scale_count (count_gates c) n = count_gates (niter n c) g` where `niter` sequentially composes copies of a circuit.
* `add_counts : gate_counts -> gate_counts -> gate_counts` 
  **Description:** Add gate counts. 
  **Verified Properties:** For any circuits `c1` and `c2`, `add_counts (count_gates c1) (count_gates c2) = count_gates (c1 ++ c2)` where `++` concatenates two circuits.
* `count_gates_lcr : circ * circ * circ -> (n : int) -> gate_counts` 
  **Description:** Compute the gates required for *n* iterations of the input LCR decomposition (see `optimize_nam_lcr`). 
  **Verified Properties:** For any circuits `l, c, r`, `count_gates_lcr` counts the gates required by one iteration of `l`, *n-2* iterations of `c`, and one iteration of `r`, i.e. `count_gates_lcr (l,c,r) n = count_gates (l ++ niter c (n-2) ++ r)`.

### Optimization

* `optimize_1q_gates : circ -> circ` 
  **Description:** Implementation of Qiskit's `SingleQubitGateCancellation` routine, which merges adjacent 1-qubit gates. Internally uses the IBM gate set. 
  **Verified properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `cx_cancellation : circ -> circ` 
  **Description:** Implementation of Qiskit's `CXCancellation` routine, which cancels adjacent CX gates. Internally uses the IBM gate set. 
  **Verified Properties:** Preserves semantics (WT), preserves WT, preserves mapping
* `optimize_ibm : circ -> circ` 
  **Description:** Run `optimize_1q_gates` followed by `cx_cancellation`. 
  **Verified properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `not_propagation  : circ -> circ` 
  **Description:** Implementation of Nam et al.'s "NOT propagation,"" which commutes X gates rightward through the circuit, cancelling them when possible. Internally uses the RzQ gate set. 
  **Verified Properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `hadamard_reduction  : circ -> circ` 
  **Description:** Implementation of Nam et al.'s "Hadamard gate reduction," which applies a series of identities to reduce the number of H gates. Internally uses the RzQ gate set. 
  **Verified Properties:** Preserves semantics (phase), preserves WT, preserves mapping
* `cancel_single_qubit_gates  : circ -> circ` 
  **Description:** Implementation of Nam et al.'s "single-qubit gate cancellation," which commutes single-qubit gates rightward through the circuit, cancelling them when possible, and reverting them to their original positions if they fail to cancel. Internally uses the RzQ gate set. 
  **Verified Properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `cancel_two_qubit_gates  : circ -> circ` 
  **Description:** Implementation of Nam et al.'s "two-qubit gate cancellation," which commutes CX gates rightward through the circuit, cancelling them when possible, and reverting them to their original positions if they fail to cancel. Internally uses the RzQ gate set. 
  **Verified Properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `merge_rotations  : circ -> circ` 
  **Description:** Implementation of Nam et al.'s "rotation merging using phase polynomials," which combines Rz gates that act on the same logical state. Internally uses the RzQ gate set.
  **Verified Properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `optimize_nam : circ -> circ` 
  **Description:** Run optimizations in the order 0, 1, 3, 2, 3, 1, 2, 4, 3, 2 where 0 is `not_propagation`, 1 is `hadamard_reduction`, 2 is `cancel_single_qubit_gates`, 3 is `cancel_two_qubit_gates`, and 4 is `merge_rotations`. 
  **Verified properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `optimize_nam_light  : circ -> circ` 
  **Description:** Run optimizations in the order 0, 1, 3, 2, 3, 1, 2, using the same numbering scheme as above. This will be faster than `optimize_nam` because it excludes rotation merging, which is our slowest optimization. 
  **Verified Properties:** Preserves semantics (WT, phase), preserves WT, preserves mapping
* `optimize_nam_lcr  : circ -> (circ * circ * circ) option`
  **Description:** Use `optimize_nam` to optimize across loop iterations. To count the gates required for $n$ iterations, use `count_gates_lcr`. 
  **Verified Properties:** Say that this function returns `Some (l, c, r)` on input `c0`. For any n >= 3, iterating `c0` n times is equivalent to running `l` once, `c` n-2 times, and `r` once (i.e. running `l ++ niter c (n-2) ++ r`). If `c0` is well-typed then `l`, `c`, `r` are also well-typed. If `c0` is properly mapped then `l`, `c`, `r` are also properly mapped.

### Circuit Mapping

Along with a quantum circuit, mapping requires an input *layout* and *connectivity graph*. The layout describes the initial mapping from logical to physical qubits. On an architecture with n physical qubits, the input layout should be a bijection on integers {0, ..., n-1}. Under the hood, a `layout` is a pair of functions `(nat -> nat) * (nat -> nat)`` that maps logical qubits to physical qubits and physical qubits back to logical qubits. A layout is *well-formed* if these two functions accurately represent a bijection and its inverse. A connectivity graph (`c_graph`) is a triple `nat * (nat -> nat -> list nat) * (nat -> nat -> bool)``, which consists of the total number of qubits in the system, a function to generate an undirected path between any two qubits, and an oracle that indicates whether a directed edge exists between two qubits. A connectivity graph is *well-formed* if the function to get paths returns a valid path between any two qubits in the system.

* `check_layout : layout -> (n : int) -> bool` 
  **Description:** Check if an input layout is a bijection on {0, ..., n-1}. 
  **Verified properties:** If this function returns true then the layout satisfies our Coq predicate for well-formedness.
* `check_graph : c_graph -> bool` 
  **Description:** Check if the input graph is well-formed. This function will make O(n^2) calls to the path finding function for $n$ qubits, so it will likely be slow. 
  **Verified properties:** If this function returns true then the graph satisfies our Coq predicate for well-formedness.
* `check_constraints : circ -> c_graph -> bool` 
  **Description:** Check if the circuit satisfies the constraints of the connectivity graph.
  **Verified properties:** This function returns true iff the circuit satisfies the constraints of the connectivity graph.
* `simple_map : (c : circ) -> layout -> (cg : c_graph) -> (circ * layout)` 
  **Description:** Return `(c', la)` where `c'` is the mapped circuit and `la` is the final layout. 
  **Verified properties:** Provided that `c` is well-typed using the dimension stored in `cg`, and `la` and `cg` are well-formed, this transformation preserves semantics (WT, perm) and preserves WT. Furthermore, the output `c'` respects constraints `cg` and the output `la` is a well-formed layout.
* `make_tenerife : unit -> c_graph}` 
  **Description:** Create a graph for IBM's 5-qubit Tenerife machine. 
  **Verified Properties:** The output connectivity graph is well-formed.
* `make_lnn : (n : int) -> c_graph` 
  **Description:** Create a 1D LNN graph with n qubits. 
  **Verified Properties:** The output connectivity graph is well-formed.
* `make_lnn_ring : (n : int) -> c_graph` 
  **Description:** Create a 1D LNN ring graph with n qubits. 
  **Verified Properties:** The output connectivity graph is well-formed.
* `make_grid : (m : int) -> (n : int) -> c_graph` 
  **Description:** Create a m x n 2D grid. 
  **Verified Properties:** The output connectivity graph is well-formed.
* `trivial_layout : (n : int) -> layout` 
  **Description:** Create a trivial layout on $n$ qubits (i.e. logical qubit i is mapped to physical qubit i). 
  **Verified Properties:** The output layout is well-formed.
* `list_to_layout : list int -> layout` 
  **Description:** Make a layout from a list. Example: the list `[3; 4; 1; 2; 0]` is transformed to a layout with physical to logical qubit mapping {0 -> 3, 1 -> 4, 2 -> 1, 3 -> 2, 4 -> 0} (so physical qubit 0 stores logical qubit 3) and the appropriate inverse logical to physical mapping. 
  **Verified Properties:** N/A
* `layout_to_list : layout -> int -> list int` 
  **Description:** Convert a layout to a list for easier printing. Example: the layout with physical to logical qubit mapping {0 -> 3, 1 -> 4, 2 -> 1, 3 -> 2, 4 -> 0} is transformed to the list `[3; 4; 1; 2; 0]`. 
  **Verified Properties:** N/A
