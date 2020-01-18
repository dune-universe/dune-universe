# OpenQASM Parser

OCaml library for manipulating [OpenQASM](https://github.com/Qiskit/openqasm) Abstract Syntax Tree

## Requirements
This library requires a recent version of OCaml, `dune` and `menhir`.

## Installation

Use the package manager [opam](https://opam.ocaml.org/) to install OpenQASM parser.

```bash
opam install openQASM
```

Or use the standard `make build` and `make install` commands.

## Usage

To play with the library, run `dune utop` and issue a command such as:

```ocaml
OpenQASM.get_ast "teleport.qasm";;
```

## Contributing
Pull requests are welcome.

## License
[MIT](https://choosealicense.com/licenses/mit/)
