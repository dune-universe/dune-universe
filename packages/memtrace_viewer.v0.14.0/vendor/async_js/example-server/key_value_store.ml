open Core
open Async
open Cohttp_async

type state = string String.Table.t

let respond_string s =
  Server.respond_string
    ~headers:(Cohttp.Header.of_list [ "Access-Control-Allow-Origin", "*" ])
    s
;;

let respond_with_error () =
  Server.respond_string
    ~status:`Not_found
    "no such endpoint"
    ~headers:(Cohttp.Header.of_list [ "Access-Control-Allow-Origin", "*" ])
;;

let state = String.Table.create ()

let main port_opt () =
  let port = Option.value ~default:8000 port_opt in
  Server.create
    ~on_handler_error:`Ignore
    (Tcp.Where_to_listen.of_port port)
    (fun ~body:_ _addr request ->
       match request.meth with
       | `GET ->
         let uri = request.resource |> Uri.of_string in
         let uri_path = Uri.path uri in
         (* This should be a post, but getting the request body was too annoying. *)
         (match uri_path with
          | "/set" ->
            let res =
              let open Option.Let_syntax in
              let%map key = Uri.get_query_param uri "key"
              and data = Uri.get_query_param uri "value" in
              printf "Setting %s %s\n" key data;
              Hashtbl.set state ~key ~data
            in
            (match res with
             | None -> respond_with_error ()
             | Some () -> respond_string (Sexp.to_string (Unit.sexp_of_t ())))
          | "/get" ->
            (match Uri.get_query_param uri "key" with
             | None -> respond_with_error ()
             | Some key ->
               respond_string
                 (Sexp.to_string ([%sexp_of: string option] (Hashtbl.find state key))))
          | _ -> respond_with_error ())
       | _ -> respond_with_error ())
  >>= fun _ -> Deferred.never ()
;;

let () =
  Command.async_spec
    ~summary:"key value store server"
    Command.Spec.(empty +> flag "-p" (optional int) ~doc:" port")
    main
  |> Command.run
;;
