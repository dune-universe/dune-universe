# Validator

[![CI](https://github.com/sporto/ocaml_validator/actions/workflows/ci.yml/badge.svg)](https://github.com/sporto/ocaml_validator/actions/workflows/ci.yml)

Create a record validator via composable sub-validators

## Installation

### Using Opam

```bash
opam install validator
```

### Using Esy

```bash
esy add @opam/validator
```

## Usage

### In OCaml

```ocaml
type input_form = {
  name: string;
  email: string option;
  age: int;
}

type valid_form = {
  name: string;
  email: string;
  age: int;
}

let build_valid_form name email age =
  { name; email; age }

let validator_name =
  let open Validator in
  string_is_not_empty "Empty"
  |> compose
    (string_has_min_length 3 "Name is too short")

let validator_email =
  let open Validator in
  option_is_some "Missing email"
  |> compose (string_is_email "Not an email")

let validator_age =
  let open Validator in
  int_min 13 "Must be 13"

let validator (input: input_form) =
  let open Validator in
  build build_valid_form
  |> validate input.name validator_name
  |> validate input.email validator_email
  |> validate input.age validator_age

validator { name = "Sam"; email = Some "sam@sample.com"; age = 20}

==>

Ok { name = "Sam"; email = "sam@sample.com"; age = 20}
```

## Contributing

Take a look at our [Contributing Guide](CONTRIBUTING.md).