open Sturgeon
open Session
open Inuit

let buffer = ref ""

let apply_change =
  let change s patch =
    let open Inuit.Patch in
    let pos1 = utf8_offset s patch.offset in
    let pos2 = utf8_offset s ~offset:pos1 (Patch.removed patch) in
    String.sub s 0 pos1 ^ Patch.inserted_text patch ^ String.sub s pos2 (String.length s - pos2)
  in
  fun txt -> buffer := change !buffer txt

let clients = ref []

let lock = Mutex.create ()
let client_change client patch =
  Mutex.lock lock;
  prerr_endline "change";
  match
    apply_change patch;
    List.iter (fun client' ->
        if client != client' then
          Inuit.Socket.send client' patch
      ) !clients
  with
  | () -> Mutex.unlock lock
  | exception exn -> Mutex.unlock lock; raise exn

let () =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let open Sexp in
  let socket_connect t () =
    prerr_endline "Client connected";
    Socket.send t (Patch.make ~offset:0 [`Editable] (Patch.Insert !buffer));
    Socket.set_receive t (client_change t);
    clients := t :: !clients
  in
  let socket_close t () =
    prerr_endline "Client disconnected";
    clients := List.filter ((!=) t) !clients
  in
  let server = Sturgeon_recipes_server.text_server "sync-text" (fun ~args shell ->
      prerr_endline "New client";
      let socket = Inuit.Socket.make ~receive:ignore in
      Socket.set_on_connected socket (socket_connect socket);
      Socket.set_on_closed socket (socket_close socket);
      let buffer = Stui.create_buffer shell ~name:"test" in
      Stui.manual_connect buffer (Socket.endpoint socket)
    )
  in
  Sturgeon_recipes_server.main_loop server
