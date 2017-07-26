open Core
open Import

include Persistent_connection_intf

module Make (Conn : T) = struct
  type address = Conn.Address.t [@@deriving sexp_of]
  type conn = Conn.t

  module Event = struct
    type t =
      | Attempting_to_connect
      | Obtained_address      of address
      | Failed_to_connect     of Error.t
      | Connected             of conn sexp_opaque
      | Disconnected
    [@@deriving sexp_of]

    type event = t

    module Handler = struct
      type t =
        { server_name     : string
        ; on_event        : event -> unit
        ; log             : Log.t option
        }
    end

    let log_level = function
      | Attempting_to_connect | Connected _ | Disconnected | Obtained_address _ -> `Info
      | Failed_to_connect _ -> `Error

    let handle t { Handler. server_name ; log ; on_event } =
      on_event t;
      Option.iter log ~f:(fun log ->
        if Log.would_log log (Some (log_level t))
        then
          Log.sexp
            log
            ~tags:[("persistent-connection-to", server_name)]
            ~level:(log_level t) (sexp_of_t t))
  end

  type t =
    { get_address    : unit -> address Or_error.t Deferred.t
    ; connect        : address -> Conn.t Or_error.t Deferred.t
    ; retry_delay    : unit -> unit Deferred.t
    ; mutable conn   : [`Ok of Conn.t | `Close_started] Ivar.t
    ; event_handler  : Event.Handler.t
    ; close_started  : unit Ivar.t
    ; close_finished : unit Ivar.t
    }
  [@@deriving fields]

  let handle_event t event = Event.handle event t.event_handler

  (* This function focuses in on the the error itself, discarding information about which
     monitor caught the error, if any.

     If we don't do this, we sometimes end up with noisy logs which report the same error
     again and again, differing only as to what monitor caught them. *)
  let same_error e1 e2 =
    let to_sexp e = Exn.sexp_of_t (Monitor.extract_exn (Error.to_exn e)) in
    Sexp.equal (to_sexp e1) (to_sexp e2)

  let try_connecting_until_successful t =
    (* We take care not to spam logs with the same message over and over by comparing
       each log message the the previous one of the same type. *)
    let previous_address = ref None in
    let previous_error   = ref None in
    let connect () =
      t.get_address ()
      >>= function
      | Error e -> return (Error e)
      | Ok addr ->
        let same_as_previous_address =
          match !previous_address with
          | None -> false
          | Some previous_address -> Conn.Address.equal addr previous_address
        in
        previous_address := Some addr;
        if not same_as_previous_address then handle_event t (Obtained_address addr);
        t.connect addr
    in
    let rec loop () =
      if Ivar.is_full t.close_started then
        return `Close_started
      else begin
        connect ()
        >>= function
        | Ok conn -> return (`Ok conn)
        | Error err ->
          let same_as_previous_error =
            match !previous_error with
            | None -> false
            | Some previous_err -> same_error err previous_err
          in
          previous_error := Some err;
          if not same_as_previous_error then handle_event t (Failed_to_connect err);
          Deferred.any [t.retry_delay (); Ivar.read t.close_started]
          >>= fun () ->
          loop ()
      end
    in
    loop ()

  let create ~server_name ?log ?(on_event = ignore) ?(retry_delay = const (sec 10.))
        ~connect get_address =
    let event_handler = { Event.Handler. server_name; log; on_event } in
    let retry_delay () = after (Time.Span.randomize ~percent:0.3 (retry_delay ())) in
    let t =
      { event_handler
      ; get_address
      ; connect
      ; retry_delay
      ; conn           = Ivar.create ()
      ; close_started  = Ivar.create ()
      ; close_finished = Ivar.create ()
      }
    in
    (* this loop finishes once [close t] has been called, in which case it makes sure to
       leave [t.conn] filled with [`Close_started]. *)
    don't_wait_for @@ Deferred.repeat_until_finished () (fun () ->
      handle_event t Attempting_to_connect;
      let ready_to_retry_connecting = t.retry_delay () in
      try_connecting_until_successful t
      >>= fun maybe_conn ->
      Ivar.fill t.conn maybe_conn;
      match maybe_conn with
      | `Close_started -> return (`Finished ())
      | `Ok conn ->
        handle_event t (Connected conn);
        Conn.close_finished conn
        >>= fun () ->
        t.conn <- Ivar.create ();
        handle_event t Disconnected;
        (* waits until [retry_delay ()] time has passed since the time just before we last
           tried to connect rather than the time we noticed being disconnected, so that if
           a long-lived connection dies, we will attempt to reconnect immediately. *)
        Deferred.choose [
          Deferred.choice ready_to_retry_connecting (fun () -> `Repeat ());
          Deferred.choice (Ivar.read t.close_started) (fun () ->
            Ivar.fill t.conn `Close_started;
            `Finished ());
        ]
    );
    t

  let connected t =
    (* Take care not to return a connection that is known to be closed at the time
       [connected] was called.  This could happen in client code that behaves like
       {[
         Persistent_connection.Rpc.connected t
         >>= fun c1 ->
         ...
           Rpc.Connection.close_finished c1
         (* at this point we are in a race with the same call inside
            persistent_client.ml *)
         >>= fun () ->
         Persistent_connection.Rpc.connected t
         (* depending on how the race turns out, we don't want to get a closed connection
            here *)
         >>= fun c2 ->
         ...
       ]}
       This doesn't remove the race condition, but it makes it less likely to happen.
    *)
    let rec loop () =
      let d = Ivar.read t.conn in
      match Deferred.peek d with
      | None ->
        begin
          d >>= function
          | `Close_started -> Deferred.never ()
          | `Ok conn -> return conn
        end
      | Some `Close_started -> Deferred.never ()
      | Some (`Ok conn) ->
        if Conn.is_closed conn then
          (* give the reconnection loop a chance to overwrite the ivar *)
          Conn.close_finished conn >>= loop
        else
          return conn
    in
    loop ()

  let current_connection t =
    match Deferred.peek (Ivar.read t.conn) with
    | None | Some `Close_started -> None
    | Some (`Ok conn) -> Some conn

  let close_finished t = Ivar.read t.close_finished
  let is_closed t      = Ivar.is_full t.close_started

  let close t =
    if Ivar.is_full t.close_started then
      (* Another call to close is already in progress.  Wait for it to finish. *)
      close_finished t
    else begin
      Ivar.fill t.close_started ();
      Ivar.read t.conn
      >>= fun conn_opt ->
      begin
        match conn_opt with
        | `Close_started -> Deferred.unit
        | `Ok conn -> Conn.close conn
      end
      >>| fun () ->
      Ivar.fill t.close_finished ()
    end
end

module Versioned_rpc = Make (struct
    module Address = Host_and_port
    type t = Versioned_rpc.Connection_with_menu.t
    let rpc_connection = Versioned_rpc.Connection_with_menu.connection
    let close          t = Rpc.Connection.close          (rpc_connection t)
    let is_closed      t = Rpc.Connection.is_closed      (rpc_connection t)
    let close_finished t = Rpc.Connection.close_finished (rpc_connection t)
  end)

module Rpc = struct

  include Make (struct
      module Address = Host_and_port
      type nonrec t = Rpc.Connection.t
      let close          t = Rpc.Connection.close          t
      let is_closed      t = Rpc.Connection.is_closed      t
      let close_finished t = Rpc.Connection.close_finished t
    end)

  (* convenience wrapper *)
  let create' ~server_name ?log ?on_event ?retry_delay ?via_local_interface ?implementations
        ?max_message_size ?make_transport ?handshake_timeout
        ?heartbeat_config get_address =
    let connect host_and_port =
      let (host, port) = Host_and_port.tuple host_and_port in
      Rpc.Connection.client ~host ~port ?via_local_interface ?implementations
        ?max_message_size ?make_transport ?handshake_timeout
        ?heartbeat_config
        ~description:(Info.of_string ("persistent connection to " ^ server_name)) ()
      >>| Or_error.of_exn_result
    in
    create ~server_name ?log ?on_event ?retry_delay ~connect get_address
end
