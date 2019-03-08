fluent-logger-ocaml
===================

A structured logger for Fluentd (OCaml)


Features
------------

* Inet/Unix domain sockets are available
* Buffering data on memory if failed to send
* Persistent connection
* Exponential backoff for reconnection

Requirement
------------
- opam
- dune

Build
------------
```
$ opam install --deps-only .
$ dune build lib/fluent_logger.a
```

Install
------------
```
$ opam install .
```

Test
------------

```
$ dune runtest
```

Usage
------------

```ocaml
let () =
  let module L = Fluent_logger in
  let logger = (* Fluent_logger.t *)
    (* INET domain socket *)
    L.create ()
    (* UNIX domain socket *)
    (* L.create_for_unix path *) 
  in
  L.post logger "production" (
    `FixMap [
      (L.of_string "id",   L.uint8_of_int i);
      (L.of_string "name", L.of_string "foobar");
      (L.of_string "age",  L.uint8_of_int 81);
      (L.of_string "pi",   L.of_float 3.14)
    ]
  );
  L.release logger
```

See also `example` directory.
You can build and execute it like this:

```
$ dune exec example/example.exe
```

