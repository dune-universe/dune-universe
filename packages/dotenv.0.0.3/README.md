# Dotenv

Small lib to allow storing config separate from code.

Dotenv loads variables from a file (named by default `.env`, hence the name) into the application environment. They can be read in `Sys.env`.

This allows for the user to reproduce the deployed environments locally, as if the application was deployed in said environments.

This way we keep the secrets out of the committed code, and the application ready to access them when deployed. Please don't commit the environment files ;)

This is a port of JavaScript's Dotenv (https://github.com/motdotla/dotenv).

I have adapted it a little, but the result should be similar enough so that nothing from the JS world is missed.

## Install

With opam:

```sh
opam install dotenv
```

## Usage

Dotenv should be the first thing to be run on you program's entry function.

Ex:

```ocaml
let _ =
  Dotenv.export () |> ignore
  let whatever_secret = Sys.get_env_opt "secret" in
  rest of the app...
```

This will read from a `.env` file at the base of your app and populate the environment with the variables that it find in export format (`VAR=value`).

### Methods

Dotenv has two methods: `parse` and `export`.

`export` will run through the env file and add the valid variables to the environment. They are then accessible via `Sys.get_env` exactly as if they were on the deployment (or development) environment when the program started.

`parse` will run through the env file and get all the variables onto an association list, so that it can then be used without having the variables on the environment. This is probably not used in most cases. Can be useful for loading files that aren't secret or for some types of debugging. Don't forget the debug option, there's a lot of good info there.

### Arguments to the functions:

`export` and `parse` have three possible arguments, all optional:

- `debug`:bool (**defaults to [false]**) -> If true, will log the operations that dotenv is executing. It's useful for debug if there's something unexpected in read variables.
- `path`:string (**defaults to [".env"]**) -> Points to where the `.env` file is. This option allows you to use env files in different dirs or to simulate different environments easily.
- `encoding`: [< Uutf.decoder_encoding ] -> Used to read files in a different encoding. We're using `Uutf` for this. Accepted values: `` [ `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 | `US_ASCII | `ISO_8859_1] ``

### Parse Rules

The parse rules used are pretty much the same as dotenv for JS - from which I partly used the text below, with some edits -, with some small differences that are presented next. The parsing will be the same for `parse` or `export` methods, but the latter will add `VAR` to the application environment with value `VALUE`:

- `VAR=value` becomes `("VAR", "value")`
- spaces are respected:`VAR=some value` becomes `("VAR", "some value")`
- empty lines are skipped
- lines beginning with `#` or that don't respect the export format are treated as comments
- empty values become empty strings: `VAR=` becomes `("VAR", "")`
- inner quotes are maintained (think JSON): `JSON={"foo": "bar"}` becomes `("JSON", "{\"foo\": \"bar\"}")`
- whitespace is removed from both ends of unquoted values: `VAR= some value` becomes `("VAR", "some value")`
- single quoted values become strings: `SINGLE_QUOTE='quoted'` becomes `(SINGLE_QUOTE, "quoted")`
- single and double quoted values maintain whitespace from both ends: `VAR=" some value "` becomes `("VAR", " some value ")`
