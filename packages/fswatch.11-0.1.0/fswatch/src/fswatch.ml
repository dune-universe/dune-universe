(*
 * fswatch.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of ocaml-fswatch.
 *)


module SessionMap = Ephemeron.K1.Make(
  struct
    include Nativeint
    let hash= Hashtbl.hash
  end)

module Status = struct
  include Stub.Status
end

module Event = struct
  include Stub.Event
end

module Monitor = struct
  include Stub.Monitor
end

module Filter = struct
  include Stub.Filter
  type monitor_filter = cmonitor_filter = {
    text : string;
    filter_type : filter_type;
    case_sensitive : bool;
    extended : bool;
  }
end

type handle= {
  session: nativeint;
  callback: Event.callback;
  mutable alive: bool;
  mutable last_status: Status.t;
}

let sessions= SessionMap.create 0

let init_library ()=
  let callback session events=
    let events= Array.map Event.t_of_raw events in
    match SessionMap.find sessions session with
    | handle-> handle.callback events
    | exception Not_found-> ()
  in
  Callback.register "callback" callback;
  match Stub.FSW.init_library () |> Stub.Status.t_of_int with
  | Stub.Status.FSW_OK as r-> r
  | _ as r-> r

let add_path handle path=
  handle.last_status <- Stub.FSW.add_path handle.session path
    |> Stub.Status.t_of_int

let add_property handle ~name ~value=
  handle.last_status <- Stub.FSW.add_property handle.session ~name ~value
    |> Stub.Status.t_of_int

let set_allow_overflow handle allow=
  handle.last_status <- Stub.FSW.set_allow_overflow handle.session allow
    |> Stub.Status.t_of_int

let set_recursive handle recursive=
  handle.last_status <- Stub.FSW.set_recursive handle.session recursive
    |> Stub.Status.t_of_int

let set_directory_only handle directory_only=
  handle.last_status <-
    Stub.FSW.set_directory_only
      handle.session
      directory_only
    |> Stub.Status.t_of_int

let set_follow_symlinks handle follow=
  handle.last_status <- Stub.FSW.set_follow_symlinks handle.session follow
    |> Stub.Status.t_of_int

let add_event_type_filter handle filter=
  let filter = Stub.Filter.event_type_filter_to_raw  filter in
  handle.last_status <- Stub.FSW.add_event_type_filter handle.session filter
    |> Stub.Status.t_of_int

let add_filter handle filter=
  handle.last_status <- Stub.FSW.add_filter handle.session filter
    |> Stub.Status.t_of_int

let start_monitor handle=
  handle.last_status <- Stub.FSW.start_monitor handle.session
    |> Stub.Status.t_of_int

let start_monitor_thread handle= Thread.create start_monitor handle

let stop_monitor handle=
  handle.last_status <- Stub.FSW.stop_monitor handle.session
    |> Stub.Status.t_of_int

let is_running handle= Stub.FSW.is_running handle.session
let destroy_session handle=
  if handle.alive then
    (handle.last_status <- Stub.FSW.destroy_session handle.session
      |> Stub.Status.t_of_int;
    handle.alive <- false;
    SessionMap.remove sessions handle.session
    )

let init_session monitor callback=
  let session= Stub.FSW.init_session monitor in
  let handle= {
    session;
    callback;
    alive= true;
    last_status= Status.FSW_OK
  } in
  SessionMap.replace sessions session handle;
  Gc.finalise destroy_session handle;
  handle

let last_error ()= Stub.FSW.last_error () |> Stub.Status.t_of_int
let last_status handle= handle.last_status

let is_verbose= Stub.FSW.is_verbose
let set_verbose= Stub.FSW.set_verbose

