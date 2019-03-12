open Async
open Core
open Cohttp
open Cohttp.Request
open Cohttp_async

let exn_handler _ =
        Server.respond_string ~status:(`Code 500) "Internal server error" 

module Routes = struct

    let callback ?vars:(args = String.Table.create ()) request body =
        let bar = String.Table.find_exn args "baz" in
        let resp = Response.make () in
        let body = Body.of_string bar in
        return (resp, body)

    let%route "/foo/bar/<baz>" = [ `GET, callback ]

    let show_website_and_user_id ?vars:(args = String.Table.create ()) req body =
        let w_id = String.Table.find_exn args "websiteId" in
        let u_id = String.Table.find_exn args "userId" in
        Server.respond_string (w_id ^ " " ^ u_id)

    let%route "/website/<websiteId>/users/<userId>" = [ `GET, show_website_and_user_id ]

    let get_x_and_y args _ =
        let x = String.Table.find_exn args "x" in
        let y = String.Table.find_exn args "y" in
        (int_of_string x, int_of_string y)
    
    let increment (x, y) =
        (x + 1, y + 1)

    let output_name (name, greeting) =
        Printf.sprintf "%d, %d" greeting name
        |> Server.respond_string
end

let router = Ocamlapi_async.create_from_modules_exn [ (module Routes) ]

let handler ~body:b _sock req =
    Ocamlapi_async.dispatch router req b

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
