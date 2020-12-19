open! Core

type ('input, 'output, 'response) t =
  | T :
      (handle_request:(Cohttp.Request.t -> 'response)
       -> apply_response_handler:(('response -> 'output) -> 'response -> 'real_output)
       -> 'input)
      -> ('input, 'output, 'response) t

module type S = sig
  type input
  type output

  val request : input -> Cohttp.Request.t
  val handle_response : Cohttp.Response.t -> Cohttp.Body.t -> output
end

module Api = struct
  open! Async

  let call_async
      (type input output)
      (module M : S with type input = input and type output = output)
      (input : input)
      : output Deferred.t
    =
    let request = M.request input in
    let%bind response, body = Cohttp_async.Client.request request in
    let%bind body = Cohttp_async.Body.to_string body >>| Cohttp.Body.of_string in
    let result = M.handle_response response body in
    return result
  ;;

  module Foo = struct
    type input = unit
    type output = int

    let request () = Cohttp.Request.make (Uri.of_string "https://google.com")
    let handle_response _response body = Cohttp.Body.to_string body |> String.length
  end

  let foo : (module S with type input = unit and type output = int) = (module Foo)
  let (_ : _) = call_async foo
end

module Test = struct
  open! Async

  type ('input, 'output) requestor =
    Cohttp.Request.t -> (Cohttp.Response.t -> Cohttp.Body.t -> 'input) -> 'output

  type 'a spec = Cohttp.Request.t * (Cohttp.Response.t -> Cohttp.Body.t -> 'a)
  type ('a, 'b) b = ('a, 'b) requestor

  let call_async : ('input, 'input Deferred.t) requestor =
   fun request handle_response ->
    let%bind response, body = Cohttp_async.Client.request request in
    let%bind body = Cohttp_async.Body.to_string body >>| Cohttp.Body.of_string in
    let result = handle_response response body in
    return result
 ;;

  let call_async' (request, handle_response) =
    let%bind response, body = Cohttp_async.Client.request request in
    let%bind body = Cohttp_async.Body.to_string body >>| Cohttp.Body.of_string in
    let result = handle_response response body in
    return result
  ;;

  let call_list : ('input, 'input list) requestor = fun _request _handle_response -> []

  let call_async_raw : (_, (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t) requestor
    =
   fun request _handle_response ->
    let%bind response, body = Cohttp_async.Client.request request in
    return (response, body)
 ;;

  let f _g = ident
  let (_ : _) = f 1
  let (_ : _) = f 'b'

  let foo ~k () =
    let request = Cohttp.Request.make (Uri.of_string "https://google.com") in
    let handle_response _response body = Cohttp.Body.to_string body |> String.length in
    k request handle_response
  ;;

  let foo' () =
    let request = Cohttp.Request.make (Uri.of_string "https://google.com") in
    let handle_response _response body = Cohttp.Body.to_string body |> String.length in
    request, handle_response
  ;;

  type 'a t = T : ('a, _) requestor -> 'a t

  module S (M : T) = struct
    type 'a t
  end

  (* let foo' ~k:(T k) () = foo ~k () *)
  (* let (_ : _) = foo ~k:{ f = call_async } () *)

  let bar ~k () =
    let request = Cohttp.Request.make (Uri.of_string "https://google.com") in
    let handle_response _response body = Cohttp.Body.to_string body in
    k request handle_response
  ;;

  let (_ : int Deferred.t) = foo ~k:call_async ()
  let (_ : int Deferred.t) = call_async' (foo' ())
  let (_ : string Deferred.t) = bar ~k:call_async ()
  let (_ : (_ * _) Deferred.t) = foo ~k:call_async_raw ()
end

type ('input, 'output) u =
  { get_request : 'input -> string
  ; handle_response : string -> 'output
  }

let f ~handle_request ~apply_response_handler a b c =
  let request = sprintf "%s-%s-%s" a b c in
  let response_handler response = sprintf "Response! %s" response in
  handle_request request |> apply_response_handler response_handler
;;

let h f =
  let handle_request _request = "response" in
  let apply_response_handler f x = f x in
  f ~handle_request ~apply_response_handler
;;

let just_get_request f =
  let handle_request request = request in
  let apply_response_handler f x = f x in
  f ~handle_request ~apply_response_handler
;;

let x = h f

let g ~handle_request ~apply_response_handler a b c =
  let request = sprintf "%d-%d-%d" a b c in
  let response_handler response = [ response; response ] in
  handle_request request |> apply_response_handler response_handler
;;

let y = h g
