# Jsonxt - JSON parsers for files, strings and more

*Jsonxt* provides a number of JSON parsers and writers for
RFC 8259 compliant JSON as well as non-standard extensions
introduced by Yojson.  Features include

* RFC 8259 compliant when in strict and basic mode
* Performance focused especially for files and strings
* Support for standard and extended JSON tree types:
  * Strict follows a strict interpretation of RFC 8259 with all
    numbers represented as floats.
  * Basic extends the strict type to include convenience types while maintaining
    RFC compliance.  This is compatible with yojson's Basic type
  * Extended adds additional non-standard types including tuples and variants
    and is not RFC compliant. This is compatible with yojson's Safe type
* A number of different parsers including
  * A standard JSON tree parser for various sources including string, file and channel
  * A Stream parser that returns a stream of raw JSON tokens.
  * A monad based parser compatible with async
* Writers including
  * File and string writers
  * A monad based writer that is compatible with async
  * A stream writer that converts a stream of JSON tokens
* Support for streaming JSON via Stream.t
* Standard interfaces including Yojson compatibility

## API Documentation

See the [Jsonxt API documentation](https://stevebleazard.github.io/ocaml-jsonxt) for full API documentation

## Quick Start
The following covers various use cases

### Convert a string and print the internal representation

```
let () =
  let json = Jsonxt.Basic.of_string "[1,2,3]" in
  print_endline (Jsonxt.Utilities.json_to_string_repr json);;
```

### Reading a file and printing to stdout

```
let () = 
  let json = Jsonxt.Basic.of_file "test.json" in
  Jsonxt.Basic.to_channel_hum stdout json;;
```

### Using the json\_stream parser
The json\_stream parser returns a stream of json elements
rather than a json tree.  The following is example using
the `Stream.t` interface to process the stream

```
open Printf

let parse_stream_string s =
  let stream = Jsonxt.Basic_stream.stream_from_string s in
  Stream.iter
    (fun el ->
     let s = Jsonxt.Utilities.json_stream_to_string_repr el in
     printf "%s " s)
    stream;
  printf "\n"

let () =
    let json_s = {| [ { "id":10, "str":"foo" }, { "id":11, "str":"bar" } ] |} in
    parse_stream_string json_s;;
```

### Reading and writing a file using the monad functions

```
module IO = struct
  type 'a t = 'a
  let return v = v
  let (>>=) v f = f v
end

module JsonIO = Jsonxt.Basic_monad.Make(IO)

open IO
let _ =
  let ic = open_in "test.json" in
  let reader buf len = return (input ic buf 0 len) in
  let writer s = return (output_string stdout s) in
  JsonIO.read_json ~reader ()
  >>= function
    | Error err -> print_endline ("ERROR: " ^ err); return ()
    | Ok json -> JsonIO.write_json_hum ~writer json
;;
```

### Yojson compatibility and using ppx\_yojson\_conv
To use Jsonxt's Yojson compatibility module create a `yojson.ml` file in
the source directory of the project with the following contents:

```
include Jsonxt.Yojson
```

The following is an example using ppx\_yojson\_conv:

```
module Item = struct
  type t = {
    str : string
  ; cost : float
  } [@@deriving yojson]
end

module Stock = struct
  type t = {
    desc : string
  ; inventory : int
  ; backorder : int option
  ; items : Item.t list
  } [@@deriving yojson]
end

let () =
  let item1 = { Item.str = "Store Baked Beans"; cost = 1.22 } in
  let item2 = { Item.str = "Branded Baked Beans"; cost = 1.47 } in
  let stock = { Stock.desc = "Beans"; inventory = 2; backorder = Some 3; items = [item1; item2] } in
  let json = Stock.yojson_of_t stock in
  print_endline (Yojson.Safe.show json);
  print_endline (Yojson.Safe.pretty_to_string json);
```

See the examples/ppx\_yojson\_conv directory for a working example including the dune configuration

### Async example
An example of how to use the parser monad and writer with async.  Note
that async and core libraries need to be installed.

```
open Core
open Async

module Json_async = struct
  module Json_of_async = Jsonxt.Basic_monad.Make(struct
      type 'a t = 'a Deferred.t

      let return = Deferred.return
      let (>>=) = Deferred.Monad_infix.(>>=)
    end)


  let reader inc buf size =
    Reader.read inc ~len:size buf
    >>= function
    | `Eof -> return 0
    | `Ok len -> return len

  let read inc =
    let reader = reader inc in
    Json_of_async.read_json ~reader ()

  let write outc =
    let writer buf = Writer.write outc buf |> return in
    Json_of_async.write_json ~writer

end

let run () =
  Reader.open_file "./asyncdata.json"
  >>= fun inc -> Json_async.read inc
  >>= function
      | Error err -> raise (Failure err)
      | Ok json -> begin
          Json_async.write (force Writer.stdout) json
          >>= fun () -> printf "\n"; shutdown 0 |> return
        end

let () =
  ignore (run ());
  never_returns (Scheduler.go ())
```
See the examples/async directory for a working example including the dune configuration


## Performance
Performance in general is similar to Yojson for reading depending to some extent
on the input.  

Writing wise, jsonxt is similar or slightly faster depending on the type of output.
Jsonxt optimises integer values in floats and uses integer conversion which is 4-5
times faster.  This means there is very little penalty for using [`Float] to store
an integer.  The tests/perf directory contains a comparison for basic JSON input.

## Examples
The `examples` directory contains the examples from this README as well as
additional ones.  See the individual README.md files for more details.
Build the ones with no external library dependencies with

```
dune build @examples
```

## Tests
The tests directory contains a number of tests including compliance, JSON Test Suite and
decode-encode-decode validation.  Run with

```
dune build @runtest
```

Performance tests can be found in tests/perf, see the README.md in that
directory for more details.
