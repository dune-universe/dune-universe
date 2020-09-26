open Core_kernel
open Async_kernel
open Async_js
open Js_of_ocaml

let output = ref []

let get_output () =
  Caml.flush_all ();
  let res = String.concat ~sep:"" (List.rev !output) in
  output := [];
  res
;;

let promise_of_deferred d =
  let promise = Js.Unsafe.global##._Promise in
  new%js promise
    (Js.wrap_callback (fun resolve (_reject : 'a) -> don't_wait_for (d >>| resolve)))
;;

let open_connection ~uri =
  let uri = if String.is_empty uri then None else Some (Uri.of_string uri) in
  Rpc.Connection.client ?uri ()
;;

let wrap_callback f =
  Js.Unsafe.callback_with_arguments (fun arguments ->
    (match get_output () with
     | "" -> ()
     | s -> print_endline s);
    (* Wrap the deferred into a Promise to allow devtools to wait on it  *)
    promise_of_deferred
      (let%map sexp =
         match%map Monitor.try_with ~extract_exn:true (fun () -> f arguments) with
         | Ok (Ok s) -> s
         | Ok (Error e) -> Error.sexp_of_t e
         | Error exn ->
           let sexp =
             match exn with
             | Js.Error e ->
               Sexp.List
                 [ Sexp.Atom (Js.to_string e##toString)
                 ; Sexp.Atom
                     (Js.Optdef.case e##.stack (fun () -> "no-stack") Js.to_string)
                 ]
             | (_ : Exn.t) -> Exn.sexp_of_t exn
           in
           [%sexp "escaped", (sexp : Sexp.t)]
       in
       let sexp =
         match get_output () with
         | "" -> sexp
         | s -> Sexp.List [ sexp; Atom s ]
       in
       Js.string (Sexp.to_string sexp)))
;;

let () =
  Async_js.init ();
  Sys_js.set_channel_flusher stdout (fun s -> output := s :: !output);
  Dom_html.window##.onload
  := Dom.handler (fun (_ : 'a #Dom.event Js.t) ->
    let (_ : Dom.event_listener_id) =
      Dom_html.addEventListener
        Dom_html.window
        Dom_html.Event.error
        (Dom.handler (fun e ->
           print_endline
             (sprintf
                "Error: %s: %s"
                (Js.to_string e##._type)
                (Js.Optdef.case
                   (Obj.magic e)##.message
                   (fun () -> "no message")
                   Js.to_string));
           Js._false))
        Js._false
    in
    (* We export a javascript callback that will then be triggered from the server side. *)
    Js.export
      Async_js_test_lib.Callback_function.(name (Open_rpc_and_wait ()))
      (wrap_callback (fun arguments ->
         let uri = Js.Unsafe.get arguments 0 |> Js.to_string in
         let open Deferred.Or_error.Let_syntax in
         let%bind connection = open_connection ~uri in
         let%bind (_ : string) =
           Rpc.Rpc.dispatch
             Async_js_test_lib.Rpcs.send_string
             connection
             "some string"
         in
         (* Wait a bit so that the connection is not immediately closed *)
         let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_sec 1.0) in
         Deferred.Or_error.return [%sexp "OK from client"]));
    (* The server side will wait for the text 'Ready' to appear and only then start the testing. *)
    Dom_html.document##.body##.textContent := Js.Opt.return (Js.string "Ready");
    Js._true)
;;
