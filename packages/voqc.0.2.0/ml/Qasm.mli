(** Read an OpenQASM file and return a circuit along with the number of qubits it uses. *)
val read_qasm :  string -> Main.circ * int

(** Write a circuit to an OpenQASM file. The second argument is the number of qubits used in the circuit. *)
val write_qasm : Main.circ -> int -> string -> unit
