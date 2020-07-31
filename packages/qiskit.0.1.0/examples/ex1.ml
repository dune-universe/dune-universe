open Qiskit

(* Create the circuit *)
let qc = quantum_circuit 2 2 
  |> h 0 
  |> id 1 
  |> barrier
  |> cx 0 1
  |> barrier
  |> measure 0 0
  |> measure 1 1
  |> draw;;

(* Start a simulation *)
Aer.get_backend "qasm_simulator" 
  |> execute qc 
  |> result 
  |> get_counts 
  |> Visualization.plot_histogram;

(* Run the circuit on real quantum hardware *)
(* let prov = IBMQ.enable_account "IBMQ_API_KEY" in
let j = Provider.get_backend "ibmq_london" prov |> execute qc in
Tools.Monitor.job_monitor j;
j 
  |> result 
  |> get_counts 
  |> Visualization.plot_histogram;; *)
