# mlvoqc

This repository contains OCaml code for running the VOQC quantum circuit compiler [1]. The `.ml` files in `ml/extracted` are *extracted* from the verified Coq definitions in `VOQC` directory of [inQWIRE/SQIR](https://github.com/inQWIRE/SQIR). For instructions on how to re-generate the extracted OCaml code from our Coq definitions see [Extraction](#extraction) below. 

`voqc-cli.ml` in the top-level directory provides a simple command line interface for interacting with the VOQC compiler. Instructions for compiling and running `voqc-cli.ml` are given below. **However**, we recommend using our Python wrapper available in [inQWIRE/pyvoqc](https://github.com/inQWIRE/pyvoqc) instead. The pyvoqc repository also includes a tutorial.

Scripts to run VOQC on the benchmarks described in our paper are available in the `benchmarks` directory. See the README in that directory for more information.

## Setup

VOQC requires OCaml (version >= 4.08.1), [opam](https://opam.ocaml.org/doc/Install.html), and dune. Once you have opam installed, follow the instructions below to set up your environment.
```
# environment setup
opam init
eval $(opam env)

# install some version of the OCaml compiler in a switch named "voqc"
opam switch create voqc 4.10.0
eval $(opam env)

# install dune (needed to build VOQC)
opam install dune
```

*Notes*:
* Depending on your system, you may need to replace 4.10.0 in the instructions above with something like "ocaml-base-compiler.4.10.0". Opam error messages and warnings are typically informative, so if you run into trouble then make sure you read the console output.

## Installation

You can install the VOQC library using the opam package manager.
```
opam install voqc
```
If for some reason that doesn't work, then you can also install VOQC locally using `make install`.

Once you have the VOQC library installed, you can build the command line interface with `make voqc-cli`.

*Notes*: 
* When building the VOQC executable on a Mac, you will likely see the warning `ld: warning: directory not found for option '-L/opt/local/lib'`. This is due to zarith (see [ocaml/opam-repository#3000](https://github.com/ocaml/opam-repository/issues/3000)) and seems to be fine to ignore.

## Usage

Since the VOQC CLI is built using dune, you need to run it with `dune exec`. Here are a few examples:
```
# Run the "Nam" optimizations on input program <inf> and write the output to <outf>
dune exec -- ./voqc_cli.exe -i inf -o outf -optimize-nam

# Run the "Nam" and "IBM" optimizations on input program <inf> and write the output to <outf>
dune exec -- ./voqc_cli.exe -i inf -o outf -optimize-nam -optimize-ibm

# list all available options
dune exec -- ./voqc_cli.exe --help
```

VOQC supports OpenQASM programs that use the following gates:
* I, X, Y, Z, H, S, T, Sdg, Tdg
* Rx(f), Ry(f), Rz(f)
* Rzq(n,n)
* U1(f), U2(f,f), U3(f,f,f)
* CX, CZ, SWAP
* CCX, CCZ
where n is an integer expression and f is a float expression. rzq is a non-standard gate that we have defined specifically for VOQC. rzq(num,den) performs a rotation about the z-axis by ((num /den) * pi) for integers num and den. VOQC currently does not support OpenQASM programs that use measurement.

## Documentation

Documentation for the VOQC library (generated with [odoc](https://github.com/ocaml/odoc)) is available [here](https://inQWIRE.github.io/mlvoqc/voqc/index.html).

## Extraction

To re-generate the extracted OCaml code (e.g. when you want to update to include new features from [inQWIRE/SQIR](https://github.com/inQWIRE/SQIR)), change into the `extraction` directory and run `./extract.sh`. This will run Coq on our `Extraction.v` file and move the generated OCaml code to the correct directory. Depending on updates made to the Coq code, you may need to modify `extract.sh` or `Extraction.v`.

In order to perform extraction, you will need to have Coq installed (`opam install coq`). Extraction has only been tested with Coq version 8.12.0.

*Notes*:

For performance, we have decided to:
* Extract Coq nat to OCaml int.
* Extract Coq Q to OCaml Zarith Q.
* Replace Coq's FMapAVL and FSetAVL implementations with OCaml's built-in Set and Map.

This makes the assumption that these OCaml data structures satify the properties proved about their corresponding Coq implementations. Note that nats are only used to identify qubit indices and we do not perform arithmetic over qubit indices, so an overflow is unlikely.

Perhaps more problematic, we have decided to extract Coq's axiomatized Reals (used for continuous gate parameters) to OCaml floats. This invites the possibility of floating point rounding error, which is not accounted for in our proofs. We have not observed errors cause by this during testing, but it's something to keep in mind. We are working to come up with a better solution. To avoid this potentially-buggy feature, you can avoid the rz, u1, u2, and u3 gates in favor of the rzq gate, whose parameter is described using OCaml multi-precision rational numbers. Also, do not use the optimize_ibm, optimize_1q_gates, or cx_cancellation functions as they will interally convert to u1, u2, and u3 gates.

## Contributing

Pull requests are welcome! But note that the code in `ml/extracted` should *only* be updated following the instructions in [Extraction](#extraction) above. If you are interested in developing verified optimizations for quantum circuits, then consider working from our Coq development at [inQWIRE/SQIR](https://github.com/inQWIRE/SQIR) instead.

## Acknowledgements

This project is supported by the U.S. Department of Energy, Office of Science, Office of Advanced Scientific Computing Research, Quantum Testbed Pathfinder Program under Award Number DE-SC0019040.

## References

[1] Kesha Hietala, Robert Rand, Shih-han Hung, Xiaodi Wu, and Michael Hicks. *A Verified Optimizer for Quantum Circuits*. POPL 2021. [https://dl.acm.org/doi/10.1145/3434318](https://dl.acm.org/doi/10.1145/3434318).
