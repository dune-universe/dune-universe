open! Core
open! Async
include Httpaf

module Body = struct
  include Httpaf.Body

  let to_string body =
    let ivar = Ivar.create () in
    let result = Queue.create () in
    let rec on_read bs ~off ~len =
      Queue.enqueue result (Bigstring.to_string bs ~pos:off ~len);
      Httpaf.Body.schedule_read body ~on_eof ~on_read
    and on_eof () = Ivar.fill ivar (Queue.to_list result |> String.concat) in
    Httpaf.Body.schedule_read body ~on_eof ~on_read;
    Ivar.read ivar
end

module Server = struct
  type t = Tcp.Server.inet

  module Response_body = struct
    type t = String of string | Pipe of string Pipe.Reader.t
  end

  type response = Response.t * Response_body.t

  type request_handler =
    body:[ `read ] Body.t ->
    Socket.Address.Inet.t ->
    Request.t ->
    response Deferred.t

  let respond_with_pipe ?headers ?(status = `OK) pipe =
    Deferred.return (Response.create ?headers status, Response_body.Pipe pipe)

  let respond_with_file ?headers ?status filename =
    let%bind reader = Reader.open_file filename in
    respond_with_pipe ?headers ?status (Reader.pipe reader)

  let respond_string ?headers ?(status = `OK) str =
    Deferred.return (Response.create ?headers status, Response_body.String str)

  let respond_with_redirect ?(headers = Headers.empty) uri =
    let headers =
      Headers.add_unless_exists headers "location" (Uri.to_string uri)
    in
    respond_string ~headers ~status:`Found ""

  let create ~on_handler_error where_to_listen handler =
    Httpaf_async.Server.create_connection_handler
      ~error_handler:on_handler_error ~request_handler:(fun addr reqd ->
        don't_wait_for
          (let%bind (response : Response.t), response_body =
             handler ~body:(Reqd.request_body reqd) addr (Reqd.request reqd)
           in
           match (response_body : Response_body.t) with
           | String str ->
               let response =
                 {
                   response with
                   headers =
                     Headers.add response.headers "content-length"
                       (Int.to_string (String.length str));
                 }
               in
               Reqd.respond_with_string reqd response str;
               return ()
           | Pipe pipe ->
               let response =
                 {
                   response with
                   headers =
                     Headers.add response.headers "transfer-encoding" "chunked";
                 }
               in
               let body = Reqd.respond_with_streaming reqd response in
               let%bind () =
                 Pipe.iter pipe ~f:(fun str ->
                     if Body.is_closed body then (
                       Pipe.close_read pipe;
                       return () )
                     else (
                       Body.write_string body str;
                       let ivar = Ivar.create () in
                       Body.flush body (fun () -> Ivar.fill ivar ());
                       Ivar.read ivar ))
               in
               Body.close_writer body;
               return ()))
    |> Tcp.Server.create_sock
         ~on_handler_error:
           (`Call
             (fun addr exn ->
               print_s
                 [%message
                   "handler error" (addr : Socket.Address.Inet.t) (exn : Exn.t)]))
         where_to_listen
end

module Cookie = Cookie
module Accept = Accept
