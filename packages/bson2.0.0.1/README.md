# Bson2

Bson2 is an Ocaml library for Bson encoding and decoding.

The Bson spec can be found here: [Bson](http://bsonspec.org).

## Modules

Currently, this library supports reading and writing from and to the binary bson format.

`Bson2.Binary` contains the `Reader` and `Writer` modules.

## Getting Started

### Writing

```ocaml

let document =
    let open Bson2.Binary.Writer in
    let doc = create 64 in
    write_string doc "name" "Stephanos";
    write_int32 doc "age" 28l;
    write_string doc "email" "stephanos.tsoucas@gmail.com";
    write_document_close doc;
    to_string doc |> Result.ok_or_failwith

```

### Reading

A reader can be created from either bytes or a channel.

To read a document, call the `read_next` function.

```ocaml

open Bson2.Binary

let process reader =
    match Reader.read_next reader with
    | Field(name, Double f) ->
        handle_float name f
    | Field(name, String s) ->
        handle_string name s
    | Field(name, Document_start) -> handle_document_start name
    | Document_end -> handle_document_end ()
    ...

let () =
    let chan = Stdio.In_channel.create "foo.txt" in
    let reader = Reader.of_channel chan in
    process reader
```
