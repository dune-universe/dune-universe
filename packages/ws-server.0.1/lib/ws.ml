module Client = struct
  module Id = struct
    type t = int

    let to_int x = x

    let of_int x = x

    let ( = ) = ( = )

    let compare = compare
  end

  type t = { id: Id.t; client: Websocket_lwt_unix.Connected_client.t }

  let id { id; _ } = id

  let send { client; _ } content =
    let open Websocket in
    let frame = Frame.(create ~content ~final:true ~opcode:Opcode.Text ()) in
    let open Websocket_lwt_unix in
    Connected_client.send client frame

  let send_multiple { client; _ } content_list =
    let open Websocket in
    let frames =
      List.map
        (fun content ->
          Frame.(create ~content ~final:true ~opcode:Opcode.Text ()))
        content_list
    in
    let open Websocket_lwt_unix in
    Connected_client.send_multiple client frames
end

module Server = struct
  type status = Open | Closed

  type t = {
    port: int;
    next_id: int ref;
    next_id_lock: Lwt_mutex.t;
    clients:
      (int, Websocket_lwt_unix.Connected_client.t * status ref) Hashtbl.t;
    server: unit Lwt.t option;
    on_connect: (Client.t -> unit Lwt.t) option;
    on_close: (Client.t -> unit Lwt.t) option;
  }

  let create ~port =
    let next_id = ref 1 in
    let next_id_lock = Lwt_mutex.create () in
    let clients = Hashtbl.create 16 in
    {
      port;
      next_id;
      next_id_lock;
      clients;
      server = None;
      on_connect = None;
      on_close = None;
    }

  let run { port; next_id; next_id_lock; clients; _ } ?on_connect ?on_close
      ?on_client_error handler =
    let server =
      let open Websocket_lwt_unix in
      establish_server
        ~mode:(`TCP (`Port port))
        ~check_request:(fun _ -> true)
        (fun connected_client ->
          let%lwt () = Lwt_mutex.lock next_id_lock in
          let client_id = !next_id in
          next_id := client_id + 1;
          Lwt_mutex.unlock next_id_lock;
          let client_status = ref Open in
          Hashtbl.add clients client_id (connected_client, client_status);
          let client = Client.{ id = client_id; client = connected_client } in
          let%lwt () =
            match on_connect with Some f -> f client | None -> Lwt.return_unit
          in

          let rec loop () =
            Lwt.catch
              (fun () ->
                let%lwt frame = Connected_client.recv connected_client in
                let open Websocket in
                match frame.opcode with
                | Frame.Opcode.Ping ->
                    Connected_client.send connected_client
                      (Frame.create ~opcode:Frame.Opcode.Pong ())
                | Frame.Opcode.Text | Frame.Opcode.Binary ->
                    let%lwt () = handler client frame.content in
                    loop ()
                | Frame.Opcode.Close -> (
                    let%lwt () =
                      match !client_status with
                      | Open ->
                          (* Client initiated close *)
                          Hashtbl.remove clients client_id;
                          let open Astring in
                          if String.length frame.content >= 2 then
                            Connected_client.send connected_client
                            @@ Frame.create ~opcode:Frame.Opcode.Close
                                 ~content:
                                   String.(
                                     sub ~start:0 ~stop:2 frame.content
                                     |> Sub.to_string)
                                 ()
                          else
                            Connected_client.send connected_client
                            @@ Frame.close 1000
                      | Closed ->
                          (* Client is responding to us closing *)
                          Lwt.return_unit
                    in
                    match on_close with
                    | Some f -> f client frame.content
                    | None -> Lwt.return_unit )
                | _other -> Lwt.return_unit)
              (fun exn ->
                Hashtbl.remove clients client_id;
                match on_client_error with
                | Some f -> f client exn
                | None -> Lwt.return_unit)
          in
          loop ())
    in
    server

  let clients { clients; _ } =
    Hashtbl.fold
      (fun id (client, _status) accum -> { Client.id; client } :: accum)
      clients []

  let get { clients; _ } id =
    match Hashtbl.find_opt clients id with
    | Some (client, _status) -> Some { Client.id; client }
    | None -> None

  let broadcast server message =
    Lwt_list.iter_p (fun client -> Client.send client message) (clients server)

  let broadcast_to_others server source_client message =
    Lwt_list.iter_p
      (fun client ->
        if not Client.(Id.(id client = id source_client)) then
          Client.send client message
        else Lwt.return_unit)
      (clients server)

  let close { clients; _ } Client.{ id; client } =
    match Hashtbl.find_opt clients id with
    | None -> (* Already deleted *) Lwt.return_unit
    | Some (_, client_status) ->
        Hashtbl.remove clients id;
        let open Websocket_lwt_unix in
        let open Websocket in
        let%lwt () = Connected_client.send client (Frame.close 1000) in
        client_status := Closed;
        Lwt.return_unit

  let close_all server =
    Lwt_list.iter_p (fun client -> close server client) (clients server)

  let current_connections { clients; _ } = Hashtbl.length clients
end
