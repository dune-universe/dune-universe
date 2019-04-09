(** A simple hello world scgi * * E.g. ./_build/hello_scgi.byte *)

open Scgi

let () =
  (* Command line options *)
  let port = ref 1025 in
  let addr = ref "127.0.0.1" in
  let req_count = ref 0 in
  Arg.parse
    (Arg.align
       [ ( "--port"
         , Arg.Set_int port
         , Printf.sprintf " port number (default: %d)" !port )
       ; ( "--addr"
         , Arg.Set_string addr
         , Printf.sprintf " ip address to bind (default: %s)" !addr ) ])
    (fun s -> failwith (Printf.sprintf "Unknown argument: [%s]" s))
    "try --help" ;
  (* Start the handler *)
  let _server =
    Server.handler_inet !addr !port (fun r ->
        incr req_count ;
        let s = string_of_int !req_count in
        let body =
          Printf.sprintf
            "%s. Hello world from Ocaml SCGI. The request path is: %s" s
            (Request.path r)
        in
        Lwt.return
          { Response.status= `Ok
          ; headers= [`Content_type "text/plain"]
          ; body= `String body } )
  in
  (* Run forever in foreground. *)
  Lwt_main.run (fst (Lwt.wait ()))
