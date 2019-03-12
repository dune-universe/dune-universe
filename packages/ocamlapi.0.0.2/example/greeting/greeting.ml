open Async
open Core
open Cohttp_async
open Ocamlapi_async 

let exn_handler _ =
        Server.respond_string ~status:(`Code 500) "Internal server error" 

let greeting_route = Ocamlapi_router.Route.create
                     "/<name>/greet"
                    [`GET, fun args _request _body ->
                           Rule.RouteArgSet.find_exn args "name"
                           |> Printf.sprintf "Hello, %s!"
                           |> Server.respond_string ]

let r = Ocamlapi_router.create [ greeting_route ] exn_handler

let handler ~body:b _sock req =
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
