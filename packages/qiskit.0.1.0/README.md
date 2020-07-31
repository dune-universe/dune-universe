# Caml_qiskit

An OCaml wrapper for IBM Qiskit quantum computing toolkit.


## Example

```ocaml
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
```

![Quantum circuit](https://raw.githubusercontent.com/dakk/caml_qiskit/master/media/readme_circuit.png)

```ocaml
(* Start a simulation *)
Aer.get_backend "qasm_simulator" 
  |> execute qc 
  |> result 
  |> get_counts 
  |> Visualization.plot_histogram;
```

![Quantum sim res](https://raw.githubusercontent.com/dakk/caml_qiskit/master/media/readme_ressim.png)


```ocaml
(* Run the circuit on real quantum hardware *)
let prov = IBMQ.enable_account "IBMQ_API_KEY" in
let j = Provider.get_backend "ibmq_london" prov |> execute qc in
Tools.Monitor.job_monitor j;
j 
  |> result 
  |> get_counts 
  |> Visualization.plot_histogram;;
```

![Quantum sim res](https://raw.githubusercontent.com/dakk/caml_qiskit/master/media/readme_resreal.png)



## Install

```opam install qiskit```

You also need to install these python libraries (via pip):

- matplotlib
- numpy
- qiskit


## License
```
Copyright (c) 2020 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```