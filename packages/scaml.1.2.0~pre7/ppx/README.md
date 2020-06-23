# scaml.ppx

Make SCaml work with Dune build system and Merlin IDE.

## Limitation of the standalone compiler

SCaml's standalone compiler `scamlc' is a simple way to compile smart contracts but some drawbacks:

Hard to use within Dune build system
:    Dune is specialized for OCaml and the compilers must be `ocamlc` or `ocamlopt`.  It does not know about `scamlc` therefore it is hard to write build rules for it.

SCaml specific errors are not found in IDE
:    Merlin IDE shows OCaml errors on the fly, but it could not show SCaml specific errors.  You had to run `scamlc` to find them out.

Smart contract code and non contract code could not coexist
:    It was impossible to have non contract code together with contract code.  Contract codes had to be in independent modules.  Tests and tool functions in OCaml must be written in other modules.

## scaml.ppx makes SCaml seamless in the OCaml eco-system

`scaml.ppx`, a PPX for SCaml, tries to solve the drawbacks above:

Construct smart contracts within Dune
:    Multiple smart contract modules are now compiled by Dune without having complex rules to build.

     Smart contract codes in OCaml source files are type-checked then precompiled by SCaml compiler library.  The precompiled smart contract codes are stored in the OCaml object files.

     The precompiled smart contract codes are linked together to an OCaml executable by Dune.  To link and emit the smart contract, the executable calls `SCaml_compiler_lib.SCamlPPX.emit`.
     
IDE is aware of SCaml specific errors
:    `scaml.ppx` checks SCaml specific errors and report them to Merlin.  Many of SCaml specific errors now can be detected within the IDE.

Smart contract code and non contract code can coexist
:    Modules without `[@@@SCaml]` attributes are ignored by SCaml compilation by `scaml.ppx`.  You can now have smart contract codes and non contract codes within one module.

## How to use `scaml.ppx`

Example: `tests/`

### Code

The code for a smart contract must be written in a structure with an attribute `[@@@SCaml]`: 

```
(* my_smart_contract.ml *)
module Contract = struct
  [@@@SCaml]
  let [@entry] default () () = [], ()
end
```

To emit the smart contract in Michelson, `SCaml_compiler_lib.SCamlPPX.emit` must be called:

```
let () = SCaml_compiler_lib.SCamlPPX.emit "my_smart_contract.tz"
```

This call must be at the last part of the entire program; the smart contract codes linked later than the call of `emit` will be ignored.

The full code here:

```
(* my_smart_contract.ml *)
module Contract = struct
  [@@@SCaml]
  let [@entry] default () () = [], ()
end

let () = SCaml_compiler_lib.SCamlPPX.emit "my_smart_contract.tz"
```

The example is so simple as everything is in one module.  Smart contract codes can exist in more than one structures and in more than one source files.

### Dune

#### `wrapped-executables false`

For Dune 2.0 or newer, `(wrapped-executables false)` MUST BE SPECIFIED in the `dune-project` file.  Otherwise `SCamlc.Ppx.emit` fails complaining about `Error: [ESCaml010] Variable not found: Dune__exe....`:

Your `dune-project` file should look like:
```
(lang dune 2.x)
(name myproject) ;; Name of your porject
(wrapped_executables false)  ;; You NEED this!!!
```

#### Build an executable

* Use libraries `typerep`, `scaml.scamlib` and `scaml.compiler`
* Preprocessor: `(staged_pps ppx_typerep_conv scaml.ppx)`.  `staged_pps` is mandatory.

`typerep` and `ppx_typerep_conv` are not mandatory but they are useful to convert SCaml and Michelson values:

```
(executable
  (name my_smart_contract)
  (libraries typerep scaml.scamlib scaml.compiler)
  (preprocess (staged_pps ppx_typerep_conv scaml.ppx)))
```

#### Rule to emit Michelson

The smart contract is emitted by executing the executable:

```
(rule
 (target my_smart_contract.tz)
 (deps ./my_smart_contract.exe)
 (action (run ./my_smart_contract.exe)))
```

### Merlin

Once `dune build` tries to build `my_smart_contract.exe`, `.merlin` file for the directory should be built by `dune` automatically.
