# mlvoqc

This repository contains OCaml code for running the VOQC quantum circuit compiler [1]. The code in `ml/extracted` (excluding the files FMapAVL.ml, FSetAVL.ml, and OrderedTypeEx.ml) is *extracted* from the verified Coq definitions in `VOQC` directory of [inQWIRE/SQIR](https://github.com/inQWIRE/SQIR). FMapAVL.ml, FSetAVL.ml, and OrderedTypeEx.ml are simple handwritten wrappers around OCaml's map and set datatypes, allowing us to avoid extracting Coq's built-in definitions of maps and sets. The Qasm.ml file in `ml` contains (unverified) code for parsing OpenQASM files. For instructions on how to re-generate the extracted OCaml code from our Coq definitions see [Extraction](#extraction) below. 

`voqc.ml` in the top-level directory provides a simple command line interface for interacting with the VOQC compiler. Instructions for compiling and running `voqc.ml` are given below. **However**, we recommend using our Python wrapper available in [inQWIRE/pyvoqc](https://github.com/inQWIRE/pyvoqc) instead. The pyvoqc repository also includes a tutorial.

Scripts to run VOQC on the benchmarks described in our paper are available in the `benchmarks` directory. See the README in that directory for more information.

## Compiling & Running VOQC

Dependencies:
  * OCaml version >= 4.08.1 
  * zarith (`opam install zarith`)
  * dune (`opam install dune`)
  * OpenQASM parser (`opam install openQASM`)
  
To install our dependencies we recommend using [opam](https://opam.ocaml.org/doc/Install.html). A typical workflow on a new computer is:
  1. Install opam
  2. Set up a new switch with a recent version of OCaml (e.g. `opam switch create voqc 4.10.0`)
  3. Install dependencies with `opam install dune zarith openQASM`

*Notes*: 
* If you are building the voqc executable or library on a Mac, you will likely see the warning `ld: warning: directory not found for option '-L/opt/local/lib'`. This is due to zarith (see [ocaml/opam-repository#3000](https://github.com/ocaml/opam-repository/issues/3000)) and seems to be fine to ignore.

Then to compile `voqc.ml`, run `dune build voqc.exe` in the top-level directory.


## Running VOQC

Here are some examples of running the VOQC executable:
```
# Run the "Nam" optimizations on input program <inf> and write the output to <outf>
dune exec -- ./voqc.exe -i inf -o outf -optimize-nam

# Run the "Nam" and "IBM" optimizations on input program <inf> and write the output to <outf>
dune exec -- ./voqc.exe -i inf -o outf -optimize-nam -optimize-ibm

# list all available options
dune exec -- ./voqc.exe --help
```

VOQC supports OpenQASM programs that use the following gates:
* I, X, Y, Z, H, S, T, Sdg, Tdg
* Rx(f), Ry(f), Rz(f)
* Rzq(n,n)
* U1(f), U2(f,f), U3(f,f,f)
* CX, CZ, SWAP
* CCX, CCZ
where n is an integer expression and f is a float expression. rzq is a non-standard gate that we have defined specifically for VOQC. rzq(num,den) performs a rotation about the z-axis by ((num /den) * pi) for integers num and den. VOQC currently does not support OpenQASM programs that use measurement.

For additional detail about the functionality provided by VOQC, see the [API](API.md).

## Extraction

To re-generate the extracted OCaml code (e.g. when you want to update to include new features from [inQWIRE/SQIR](https://github.com/inQWIRE/SQIR)), change into the `extraction` directory and run `./extract.sh`. This will run Coq on our `Extraction.v` file and move the generated OCaml code to the correct directory. In order to perform extraction, you will need to have Coq installed (`opam install coq`). Extraction has only been tested with Coq version 8.12.0.

### Notes

For performance, we have decided to:
* Extract Coq nat to OCaml int.
* Extract Coq Q to OCaml Zarith Q.
* Replace Coq's FMapAVL and FSetAVL implementations with OCaml's built-in Set and Map.

This makes the assumption that these OCaml data structures satify the properties proved about their corresponding Coq implementations. Note that nats are only used to identify qubit indices and we do not perform arithmetic over qubit indices, so an overflow is unlikely.

Perhaps more problematic, we have decided to extract Coq's axiomatized Reals (used for continuous gate parameters) to OCaml floats. This invites the possibility of floating point rounding error, which is not accounted for in our proofs. We have not observed errors cause by this during testing, but it's something to keep in mind. We are working to come up with a better solution. To avoid this potentially-buggy feature, you can avoid the rz, u1, u2, and u3 gates in favor of the rzq gate, whose parameter is described using OCaml multi-precision rational numbers. Also, do not use the optimize_ibm, optimize_1q_gates, or cx_cancellation functions as they will interally convert to u1, u2, and u3 gates.

## Acknowledgements

This project is supported by the U.S. Department of Energy, Office of Science, Office of Advanced Scientific Computing Research, Quantum Testbed Pathfinder Program under Award Number DE-SC0019040.

## References

[1] Kesha Hietala, Robert Rand, Shih-han Hung, Xiaodi Wu, and Michael Hicks. *A Verified Optimizer for Quantum Circuits*. POPL 2021. [https://dl.acm.org/doi/10.1145/3434318](https://dl.acm.org/doi/10.1145/3434318).