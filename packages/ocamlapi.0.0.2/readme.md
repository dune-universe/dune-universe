# Ocamlapi

Ocamlapi is an Ocaml library for path-based routing of HTTP requests.

Full documentation is available [here](https://nosman.github.io/Ocamlapi/).

It is built on top of [Cohttp](https://github.com/mirage/ocaml-cohttp).

## Libraries

Ocamlapi ships with 4 libraries:

* ocamlapi: Base module for the library.
* ocamlapi_async: Use ocamlapi with the `Cohttp-async` backend.
* ocamlapi_lwt_unix: Use ocamlapi with the `Cohttp-lwt-unix` backend.
* ocamlapi_ppx: Syntax extensions. These eliminate boilerplate when creating routes.

## Getting Started

### Using vanilla Ocamlapi 

We will use `Ocamlapi_async` for this introduction.

Here is an example of a server that supports a single GET operation on the path:
`/<name>/greet`:

```ocaml
open Async
open Core
open Cohttp_async
open Ocamlapi_async

(* Declare a route *)
let greeting_route = Ocamlapi_router.Route.create
                     "/<name>/greet"
                    [`GET, fun args _request _body ->
                           Rule.RouteArgSet.find_exn args "name"
                           |> Printf.sprintf "Hello, %s!"
                           |> Server.respond_string ]

let exn_handler _ =
    Server.respond_string ~status:(`Code 500) "Internal server error"

(* Declare the router *)
let r = Ocamlapi_router.create [ greeting_route ] exn_handler

let handler ~body:b _sock req =
    (* Dispatch a request to a route *)
    Ocamlapi_router.dispatch r req b
    |> function
       | Some resp -> resp
       | None -> Server.respond_string ~status:(`Code 404) "404 not found"

let start_server port () =
    eprintf "Listening for HTTP on port %d\n" port;
    Cohttp_async.Server.create
                        ~on_handler_error:`Ignore
                        (Async.Tcp.Where_to_listen.of_port port)
                        handler
                        >>= fun _ -> Deferred.never ()

let () =
    let module Command = Async_extra.Command in
        Command.async_spec
                ~summary:"Start a hello world Async server"
                Command.Spec.(
                        empty +>
                        flag "-p" (optional_with_default 8080 int)
                                ~doc:"int Source port to listen on"
                ) start_server
        |> Command.run

```

For documentation around the server component of this example, consult the
[Cohttp](https://github.com/mirage/ocaml-cohttp) project.

### Using the syntax extensions

The syntax extensions in the `Ocamlapi_ppx` ppx rewriter offer a more convenient,
declarative syntax to declare routes and routers.

```ocaml

module Counter_routes = struct

  let counter = ref 0

  let%route "/counter" =
    [ `GET, fun args _req _body ->
              !counter
              |> string_of_int
              |> Server.respond_string ]

  let%route "/counter/increment" =
    [ `POST, fun args _req _body ->
               counter := !counter + 1;
               `Code 200
               |> Server.respond ]

  let%route "/counter/decrement" =
    [ `POST, fun args _req _body ->
               counter := !counter - 1;
               `Code 200
               |> Server.respond ]

end

let exn_handler _ =
    Server.respond_string ~status:(`Code 500) "Internal server error"

let%router r = [ Counter_routes ], exn_handler

```

Here, a route is declared with the url path on the left, and a list of
(HTTP method, callback) pairs on the right. *Note*: any expression with the correct type can be used as the callback. The callback doesn't necessarily have to be a lambda as in this example.

The router declaration takes a list of module names with routes declared within
them.
