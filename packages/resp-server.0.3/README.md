resp-server — Servers that communicate using the Redis serialization protocol
-------------------------------------------------------------------------------
%%VERSION%%

resp-server is an OCaml library for building servers that communicate using the [Redis serialization protocol](https://redis.io/topics/protocol).

resp-server is distributed under the ISC license.

Homepage: https://github.com/zshipko/resp-server

## Installation

resp-server can be installed with `opam`:

    opam install resp-server

If you don't use `opam` consult the [`opam`](opam) file for build instructions.

## Documentation

Documentation is available [online](https://zshipko.github.io/resp-server).

To generate documentation locally run `odig odoc resp-server` - then to view run `odig doc` in the root of the project.

See `src/resp_server.mli` for the commented interface description.

## Getting started

To create a new server using `resp-server` you need to define a few modules.

As an example we can create a simple counter server that has keys with integer values that can be incremented and decremented:

1) `BACKEND` - defines the request context and client types

```ocaml
module Backend = struct
    (** This is the global request context type *)
    type t = (string, int) Hashtbl.t

    (** The client type is the per-client request context type *)
    type client = unit

    let new_client _ctx = ()
end
```

2) `AUTH` - defines authentication types

```ocaml
module Auth = struct
    type t = string
    let check t cmd =
        Array.length cmd > 0 && cmd.(0) = t
end
```

3) Use `Make` to create the new server

```ocaml
module Server = Make(Auth)(Backend)
```

4) Define some commands

```ocaml
let modify_value db args f =
    match args with
    | [| String key |] ->
        (match Hashtbl.find_opt srv key with
        | Some i -> Hashtbl.replace srv key (f i)
        | None -> Hashtbl.replace srv key (f 0)
        end;
        Server.ok)
    | _ -> Server.error "Invalid arguments"

let _incr db _cli _cmd args =
    modify_value db args (fun a -> a + 1)

let _decr db _cli _cmd args =
    modify_value db args (fun a -> a - 1)

let commands = [
    "incr", _incr;
    "decr", _decr;
]
```

4) Create and run the server

```ocaml
let main =
    let db = Hashtbl.create 16 in
    let auth = "password" in
    let srv = Server.create ~auth ~commands (`TCP (`Port 1234)) db in
    Server.run srv

let () = Lwt_main.run main
```

## Tests

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run with:

    jbuilder runtest
