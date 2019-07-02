type t =
  { mutable state: State.t [@default State.Idle]
  ; mutable timer: Timer.t [@default Timer.create (Duration.of_sec 0)]
  ; mutable work_sessions_completed: int [@default 0] }
[@@deriving make]

let next_session t (config : Config.t) () =
  let state, duration =
    match t.state with
    | Idle ->
        (State.Working, config.work_duration)
    | Working ->
        t.work_sessions_completed <- t.work_sessions_completed + 1 ;
        if
          t.work_sessions_completed > 0
          && t.work_sessions_completed mod config.number_work_sessions = 0
        then (State.Long_break, config.long_break_duration)
        else (State.Short_break, config.short_break_duration)
    | Short_break | Long_break ->
        (State.Working, config.work_duration)
  in
  Timer.set_duration duration t.timer ;
  t.state <- state ;
  Notify.notify config state

let string_of_duration duration =
  let seconds = Duration.to_sec duration mod 60 in
  let minutes = Duration.to_min duration in
  Printf.sprintf "%i:%02i" minutes seconds

let handle_get t (config : Config.t) output_channel action =
  let send_line line =
    let%lwt () = Lwt_io.write_line output_channel line in
    Lwt_io.flush output_channel
  in
  send_line
  @@
  match action with
  | "time" ->
      string_of_duration @@ Timer.time t.timer
  | "state" ->
      State.to_string t.state
  | "completed" ->
      string_of_int t.work_sessions_completed
  | "paused" ->
      string_of_bool @@ Timer.paused t.timer
  | "percentage" -> (
      let percentage d1 d2 =
        int_of_float
          ( 100.0
          *. ( float_of_int (Duration.to_sec d1)
             /. float_of_int (Duration.to_sec d2) ) )
      in
      string_of_int
      @@
      match t.state with
      | Idle ->
          0
      | Working ->
          percentage (Timer.time t.timer) config.work_duration
      | Short_break ->
          percentage (Timer.time t.timer) config.short_break_duration
      | Long_break ->
          percentage (Timer.time t.timer) config.long_break_duration )
  | _ ->
      Printf.sprintf "Error, action get/%s not supported." action

let handle_set t (config : Config.t) output_channel action =
  match action with
  | "start" ->
      ( match t.state with
      | Idle ->
          Timer.set_duration config.work_duration t.timer ;
          Timer.start t.timer ;
          t.state <- Working
      | Working | Short_break | Long_break ->
          Timer.start t.timer ) ;
      Lwt.return_unit
  | "stop" ->
      Timer.stop t.timer ; Lwt.return_unit
  | "toggle" ->
      if Timer.paused t.timer then
        match t.state with
        | Idle ->
            Timer.set_duration config.work_duration t.timer ;
            Timer.start t.timer ;
            t.state <- Working
        | Working | Short_break | Long_break ->
            Timer.start t.timer
      else Timer.stop t.timer ;
      Lwt.return_unit
  | "reset" ->
      Timer.stop t.timer ;
      t.state <- Idle ;
      t.work_sessions_completed <- 0 ;
      Lwt.return_unit
  | "restart" ->
      ( match t.state with
      | Idle ->
          Timer.set_duration (Duration.of_sec 0) t.timer
      | Working ->
          Timer.set_duration config.work_duration t.timer
      | Short_break ->
          Timer.set_duration config.short_break_duration t.timer
      | Long_break ->
          Timer.set_duration config.long_break_duration t.timer ) ;
      Lwt.return_unit
  | "skip" ->
      next_session t config () ; Timer.start t.timer ; Lwt.return_unit
  | _ ->
      Lwt_io.write_line output_channel
        (Printf.sprintf "Error, action set/%s not supported" action)

let handle_connection t config _address input_channel output_channel =
  let%lwt () = Logs_lwt.info (fun f -> f "Connection received") in
  match%lwt Lwt_io.read_line_opt input_channel with
  | Some line -> (
      let%lwt () = Logs_lwt.debug (fun f -> f "Received: %s" line) in
      match Astring.String.cut ~sep:"/" line with
      | Some ("set", action) ->
          handle_set t config output_channel action
      | Some ("get", action) ->
          handle_get t config output_channel action
      | _ ->
          (* Send some error back, potentially via the notification script. *)
          let%lwt () =
            Logs_lwt.warn (fun f ->
                f "Received invalid request, terminating connection: %s" line
            )
          in
          let%lwt () =
            Lwt_io.write_line output_channel ("Invalid request: " ^ line)
          in
          Lwt_io.flush output_channel )
  | None ->
      let%lwt () = Logs_lwt.debug (fun f -> f "Received no line") in
      Lwt.return_unit

let setup_signal_handlers stop_mvar =
  let handle_fun _ = Lwt_main.run @@ Lwt_mvar.put stop_mvar () in
  let _ = Lwt_unix.on_signal Sys.sigint handle_fun in
  let _ = Lwt_unix.on_signal Sys.sigterm handle_fun in
  ()

let run (config : Config.t) =
  if%lwt Lwt_unix.file_exists config.socket_file then
    Logs_lwt.err (fun f -> f "Socket file already exists, exiting.")
  else
    let socket_file = Unix.ADDR_UNIX config.socket_file in
    let stop_mvar = Lwt_mvar.create_empty () in
    let%lwt () = Logs_lwt.info (fun f -> f "Starting server") in
    let t = make () in
    Timer.set_callback (next_session t config) t.timer ;
    let%lwt server =
      Lwt_io.establish_server_with_client_address socket_file
        (fun address (ic, oc) -> handle_connection t config address ic oc)
    in
    setup_signal_handlers stop_mvar ;
    let%lwt () = Lwt_mvar.take stop_mvar in
    let%lwt () = Logs_lwt.info (fun f -> f "Shutting down the server.") in
    Lwt_io.shutdown_server server
