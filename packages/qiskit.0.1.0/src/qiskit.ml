Py.initialize ()

let np = Py.import "numpy"
let qk = Py.import "qiskit"
let plt = Py.import "matplotlib.pyplot"


type qcircuit = Py.Object.t

let quantum_circuit (nq: int) (nc: int): qcircuit = 
  Py.Module.get_function qk "QuantumCircuit" [| Py.Int.of_int nq; Py.Int.of_int nc |]

let measure n nto (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc "measure" [| Py.Int.of_int n; Py.Int.of_int nto |] |> ignore; qc

let ag_p3 g p1 p2 p3 n (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Float.of_float p1; Py.Float.of_float p2; Py.Float.of_float p3; Py.Int.of_int n |]

let ag_p2 g p1 p2 n (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Float.of_float p1; Py.Float.of_float p2; Py.Int.of_int n |]
  
let ag_p1 g p n (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Float.of_float p; Py.Int.of_int n |]

let ag g n (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Int.of_int n |] |> ignore; qc

let ag2 g n1 n2 (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Int.of_int n1; Py.Int.of_int n2 |] |> ignore; qc

let ag2_p1 g p n1 n2 (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Float.of_float p; Py.Int.of_int n1; Py.Int.of_int n2 |] |> ignore; qc

let ag3 g n1 n2 n3 (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc g [| Py.Int.of_int n1; Py.Int.of_int n2; Py.Int.of_int n3 |] |> ignore; qc
  
let barrier (qc: qcircuit): qcircuit = 
  Py.Module.get_function qc "barrier" [| |] |> ignore; qc

let draw (qc: qcircuit): qcircuit = 
  let s = Py.Module.get_function qc "draw" [| Py.String.of_string "mpl" |] in 
  Py.Module.get_function plt "show" [||] |> ignore;
  qc


(* gates *)
(* https://quantum-computing.ibm.com/docs/circ-comp/q-gates *)
let reset = ag "reset"
let h = ag "h"
let x = ag "x"
let y = ag "y"
let z = ag "z"
let s = ag "s"
let sdg = ag "sdg"
let t = ag "t"
let tdg = ag "tdg"
let id = ag "id"
let rx = ag_p1 "rx"
let ry = ag_p1 "ry"
let rz = ag_p1 "rz"
let u3 = ag_p3 "u3"
let u2 = ag_p2 "u2"

let crx = ag2_p1 "rx"
let cry = ag2_p1 "ry"
let crz = ag2_p1 "rz"

let cx = ag2 "cx"
let cz = ag2 "cz"
let ch = ag2 "ch"
let swap = ag2 "swap"

let ccx = ag3 "ccx"


(* aer *)
module Aer = struct 
  type backend = Py.Object.t

  let get_backend (n: string) : backend = 
    let qk_aer = Py.Module.get qk "Aer" in
    Py.Module.get_function qk_aer "get_backend" [| Py.String.of_string n |]
end

module Provider = struct 
  type qprovider = Py.Object.t

  let get_backend (n: string) (qp: qprovider) : Aer.backend = 
    Py.Module.get_function qp "get_backend" [| Py.String.of_string n |]
end

(* IBMQ *)
module IBMQ = struct 
  let enable_account (api: string): Provider.qprovider =
    let qk_ibmq = Py.Module.get qk "IBMQ" in
    Py.Module.get_function qk_ibmq "enable_account" [| Py.String.of_string api |]
end

(* simulation *)
type qjob = Py.Object.t
type qres = Py.Object.t
type qcounts = Py.Object.t

let execute (qc:qcircuit) (sim: Aer.backend): qjob = 
  Py.Module.get_function qk "execute" [| qc; sim |]


let result (qex: qjob): qres = 
  Py.Module.get_function qex "result" [| |]

let get_counts (qres: qres): qcounts = 
  Py.Module.get_function qres "get_counts" [| |]


module Tools = struct 
  module Monitor = struct 
    let job_monitor (qj: qjob): unit =
      let qk_ts = Py.Module.get qk "tools" in
      let qk_mon = Py.Module.get qk_ts "monitor" in
      Py.Module.get_function qk_mon "job_monitor" [| qj |] |> ignore
  end
end


module Visualization = struct 
  let plot_histogram (c: qcounts) = 
    let qk_vis = Py.Module.get qk "visualization" in
    let s = Py.Module.get_function qk_vis "plot_histogram" [| c |] in 
    Py.Module.get_function plt "show" [||] |> ignore
end  