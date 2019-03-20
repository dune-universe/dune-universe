# OCaml TextRazor &nbsp; [![Build status](travis-badge)](travis)

[travis-badge]: https://travis-ci.org/Richard-Degenne/ocaml-textrazor.svg?branch=master
[travis]: https://travis-ci.org/Richard-Degenne/ocaml-textrazor

An OCaml wrapper for the TextRazor API.

# Installation

```sh
opam install textrazor
```

# Usage
## Configuration

```ocaml
open Textrazor
let client = Client.create "api_key"
```

You can set additional options using `Configuration`:

```ocaml
open Textrazor
let client =
  let configuration =
    Configuration.create ~use_eu_endpoint:true ~secure:false ()
  in
  Client.create ~configuration "api_key"
```

## Analysis

Send either a text or a publicly available URL to the analysis endpoint.

```ocaml
open Textrazor
(* Analyze a text *)
let result = Client.analyze (`Text "Text to analyze") client
(* Analyze a text by URL *)
let result =
  Client.analyze (`Uri (Uri.of_string "https://www.example.com/sample.txt")) c
```

Use `Analysis.Options` to customize the analysis.

```ocaml
open Textrazor
let result =
  let options = Analysis.Options.{default with field = value} in
  Client.analyze (`Text "Text to analyze") ~options client
```

## Account

Get information about your Textrazor account.

```ocaml
open Textrazor
let result = Client.account c
```

# Contributing

- Fork the project;
- Create your feature branch (`git checkout -b my-new-feature`);
- Commit your changes (`git commit -am 'Add some feature'`);
- Push to the branch (`git push origin my-new-feature`);
- Create a [new pull request](https://github.com/Richard-Degenne/ocaml-textrazor/pulls).
