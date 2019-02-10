(** Websocket api support for the gemini trading exchange. *)

module type CSVABLE = Csvfields.Csv.Csvable

module type EVENT_CSVABLE = sig
  include CSVABLE
  val events : t list
end

module type CSV_OF_EVENTS =
sig
  module Event_type : sig include Json.S val equal : t -> t -> bool end
  type t
  val empty : t
  val add :
    t ->
    Event_type.t ->
    (module EVENT_CSVABLE) ->
    t

  val add' :
    t ->
    Event_type.t ->
    (module CSVABLE with type t = 'a) ->
    'a list -> t

  val csv_header :
    t ->
    Event_type.t -> string list option

  val get : t -> Event_type.t -> string list list
  val all : t -> (Event_type.t * string list list) list

 val write :
   ?dir:string -> t -> Event_type.t -> int

 val write_all :
   ?dir:string -> t -> (Event_type.t * int) list
end

module Csv_of_event(
    Event_type :
    sig
      include Json.S
      include Comparable with type t := t
    end) : CSV_OF_EVENTS with module Event_type = Event_type =
struct
  type t =
    (module EVENT_CSVABLE) list Event_type.Map.t

  let empty : t = Event_type.Map.empty
  module Event_type = Event_type

  let add (t:t)
     (event_type : Event_type.t)
     (module Event : EVENT_CSVABLE) =
    Event_type.Map.add_multi t ~key:event_type
      ~data:(module Event : EVENT_CSVABLE)

  let add' (t:t)
      (type event)
      (event_type : Event_type.t)
      (module Event : CSVABLE with type t = event)
      (events:event list) =
    let module E = struct include Event let events = events end in
    add t event_type (module E)

  let csv_header (t:t) event_type =
    Event_type.Map.find_multi t event_type |>
    List.hd |>
    Option.map
      ~f:(fun (module Event : EVENT_CSVABLE) -> Event.csv_header)

  let get (t:t) (event_type:Event_type.t) =
    Event_type.Map.find_multi t event_type |>
    List.concat_map ~f:
      (fun (module Event : EVENT_CSVABLE) ->
         Event.events |> List.map ~f:Event.row_of_t
      )

  let write ?dir (t:t) (event_type:Event_type.t) =
    let dir = match dir with
      | None -> Core.Unix.getcwd ()
      | Some dir -> dir in
    let filename = Filename.concat
        dir
        (sprintf "%s.csv" (Event_type.to_string event_type))
    in
    Out_channel.with_file
      ~append:true
      ~binary:false
      ~fail_if_exists:false
      filename
      ~f:
        (fun out ->
           Event_type.Map.find_multi t event_type |>
           List.fold ~init:0
             ~f:
               (fun acc (module Event : EVENT_CSVABLE) ->
                  let events = Event.events in
                  let len = List.length events in
                  Event.csv_save_out out events;
                  acc + len
               )
        )

  let all (t:t) =
    List.filter_map Event_type.all
      ~f:
        (fun e -> match (get t e) with
          | [] -> None
          | x -> Some (e,x)
        )

  let write_all ?dir (t:t) =
    List.map Event_type.all
      ~f:(fun e -> e, write ?dir t e)
end

(** Specification for a websocket channel. *)
module type CHANNEL = sig
  (** The name of the channel *)
  val name : string

  (** The channel protocol version *)
  val version : string

  (** The uri path for this channel *)
  val path : string list

  (** Authentication syle for this channel. One of
      [`Private] or [`Public]
  *)
  val authentication : [`Private | `Public]

  (** Uri arguments which are appended to the end of
      the path segment *)
  type uri_args [@@deriving sexp, enumerate]

  (** Encder from well typed uri arguments to a string
      suitable for a uri.
  *)
  val encode_uri_args : uri_args -> string

  (** Defaut uri arguments. Optional for some channels. *)
  val default_uri_args : uri_args option

  (** Repsone type of the channel. Must have sexp converters
      and a yojson parser *)
  type response [@@deriving sexp, of_yojson]


  module Event_type :
   sig include Json.S val equal : t -> t -> bool end

  module Csv_of_event :
    CSV_OF_EVENTS with module Event_type = Event_type
  (** Given a response value produce csvable events
      modularized by event type. *)
  val events_of_response :
    response -> Csv_of_event.t

  (** Query parameters for the channel *)
  type query [@@deriving sexp]

  (** Encodes queries as an http header key value pair *)
  val encode_query : query -> string * string
end

(** Creates a websocket implementation given a [Channel] *)
module Impl(Channel : CHANNEL) =
struct

(** Establishes a web socket client given configuration [Cfg]
    and optional [query], [uri_args] and [nonce] parameters.

    Produces a pipe of [Channel.response] instances.
*)
let client (module Cfg : Cfg.S)
    ?query ?uri_args ?nonce
    () =
  let query =
    Option.map query
      ~f:
        (fun l -> List.fold ~init:String.Map.empty l
           ~f:(fun map q ->
               let key, data =
                 Channel.encode_query (Channel.query_of_sexp q) in
               String.Map.add_multi map ~key ~data
              )
        |> fun map ->
        let keys = String.Map.keys map in
        List.map keys ~f:(fun k -> k, String.Map.find_multi map k)
        ) in
  let uri = Uri.make
      ~host:Cfg.api_host
      ~scheme:"https"
      ?query
      ~path:
        (String.concat ~sep:"/"
           (Channel.path
            @
            Option.(map ~f:(Channel.encode_uri_args) uri_args |> to_list)
           )
        )
      ()
      in
  Log.Global.info "Ws.client: uri=%s"
    (Uri.to_string uri);
  let host = Option.value_exn ~message:"no host in uri"
      Uri.(host uri) in
  let port = Option.value ~default:443
      Uri_services.(tcp_port_of_uri uri) in
  let scheme = Option.value ~default:"wss"
      Uri.(scheme uri) in
  let tcp_fun s r w =
    Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss" then
       (Unix.Inet_addr.of_string_or_getbyname host >>|
        Ipaddr_unix.of_inet_addr
       ) >>= fun addr ->
       Conduit_async.
         (connect
            (`OpenSSL_with_config
               (
                 "wss",
                 addr,
                 port,
                 Ssl.configure ~version:Tlsv1_2 ()
               )
            )
         )
     else return (r, w)
    ) >>= fun (r, w) ->
    let payload = `Null in
    let path = Path.to_string Channel.path in
    let%bind payload =
      match nonce with
      | None -> return None
      | Some nonce ->
      Nonce.Request.
        (make ~nonce ~request:path ~payload () >>|
         to_yojson
        )
      >>| fun s -> Yojson.Safe.to_string s |> Option.some in
    let extra_headers =
      (match Channel.authentication with
      | `Private ->
        Option.map ~f:(fun p -> Auth.(to_headers (module Cfg) (of_payload p))) payload
      | `Public -> None
      )
      |> Option.value ~default:(Cohttp.Header.init ()) in
    let r, _w = Websocket_async.client_ez
        ~extra_headers
        ~log:Lazy.(force Log.Global.log)
        ~heartbeat:Time_ns.Span.(of_int_sec 5)
        uri r w
    in
    Log.Global.info "input pipe established for channel %s" Channel.name;
    Log.Global.flushed () >>| fun () ->
      (Pipe.map r
      ~f:
        (fun s ->
           Yojson.Safe.from_string s
           |> Channel.response_of_yojson
           |> Result.ok_or_failwith
        )
      )
  in
  let hostport = Host_and_port.create ~host ~port in
  Tcp.(with_connection Where_to_connect.(of_host_and_port hostport) tcp_fun)

let handle_client addr reader writer =
  let addr_str = Socket.Address.(to_string addr) in
  Log.Global.info "Client connection from %s" addr_str;
  let app_to_ws, sender_write = Pipe.create () in
  let receiver_read, ws_to_app = Pipe.create () in
  let check_request req =
    let req_str = Format.asprintf "%a" Cohttp.Request.pp_hum req in
    Log.Global.info "Incoming connnection request: %s" req_str ;
    Deferred.return (Cohttp.Request.(uri req |> Uri.path) = "/ws")
  in
  let rec loop () =
    Pipe.read receiver_read >>= function
    | `Eof ->
      Log.Global.info "Client %s disconnected" addr_str;
      Deferred.unit
    | `Ok ({ Websocket_async.Frame.opcode;
             extension=_; final=_; content } as frame
          ) ->
      let open Websocket_async.Frame in
      Log.Global.debug "<- %s" (show frame);
      let frame', closed =
        match opcode with
        | Opcode.Ping -> Some (create ~opcode:Opcode.Pong ~content ()), false
        | Opcode.Close ->
          (* Immediately echo and pass this last message to the user *)
          if String.length content >= 2 then
            Some (create ~opcode:Opcode.Close
                          ~content:(String.sub content ~pos:0 ~len:2) ()), true
          else
          Some (close 100), true
        | Opcode.Pong -> None, false
        | Opcode.Text
        | Opcode.Binary -> Some frame, false
        | _ -> Some (close 1002), false
      in
      begin
        match frame' with
        | None ->
          Deferred.unit
        | Some frame' ->
          Log.Global.debug "-> %s" (show frame');
          Pipe.write sender_write frame'
      end >>= fun () ->
      if closed then Deferred.unit
      else loop ()
  in
  Deferred.any [
    begin Websocket_async.server ~log:Lazy.(force Log.Global.log)
        ~check_request ~app_to_ws ~ws_to_app ~reader ~writer () >>= function
      | Error err when Error.to_exn err = Exit -> Deferred.unit
      | Error err -> Error.raise err
      | Ok () -> Deferred.unit
    end ;
    loop () ;
  ]

let command =
  let spec : (_,_) Command.Spec.t =
    let open Command.Spec in
    empty
    +> Cfg.param
    +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
    +> flag "-query" (listed sexp) ~doc:"QUERY query parameters"
    +> flag "-csv-dir" (optional string)
      ~doc:"PATH output each event type to a separate csv file at PATH. \
             Defaults to current directory."
    +> anon (maybe ("uri_args" %: sexp))
 in
  let set_loglevel = function
    | 2 -> Log.Global.set_level `Info
    | 3 -> Log.Global.set_level `Debug
    | _ -> ()
  in
  let run cfg
      loglevel query (csv_dir:string option) uri_args () =
    let cfg = Cfg.or_default cfg in
    let module Cfg = (val cfg:Cfg.S) in
    Option.iter loglevel ~f:set_loglevel;
    let uri_args =
      Option.first_some
        (Option.map ~f:Channel.uri_args_of_sexp uri_args)
        Channel.default_uri_args in
    let query = match List.is_empty query with
      | true -> None
      | false -> Some query in
    let%bind nonce =
      Nonce.File.(pipe ~init:default_filename) () in
    Log.Global.info "Initiating channel %s with path %s"
      Channel.name (Path.to_string Channel.path);
    let channel_to_sexp_str response =
      Channel.sexp_of_response response |>
      Sexp.to_string_hum |>
      sprintf "%s\n" in
    let append_to_csv response =
      let events = Channel.events_of_response response in
      let all = Channel.Csv_of_event.write_all ?dir:csv_dir events in
      let tags = List.map all ~f:(fun (k, v) ->
          Channel.Event_type.to_string k,
          Int.to_string v
        ) in
      Log.Global.debug ~tags "wrote csv response events";
      () in
    let pipe_reader response =
      append_to_csv response;
      channel_to_sexp_str response in
    client (module Cfg)
      ?query ?uri_args ~nonce () >>= fun pipe ->
    Log.Global.debug "Broadcasting channel %s to stderr..." Channel.name;
      Pipe.transfer pipe
        Writer.(pipe (Lazy.force stderr))
        ~f:pipe_reader
  in
  Channel.name,
  Command.async_spec
    ~summary:(sprintf "Gemini %s %s Websocket Command"
                Channel.version Channel.name
             ) spec run
end

(** Create a websocket interface that has no request parameters *)
module Make_no_request(Channel:CHANNEL) =
struct
  include Impl(Channel)
  let client = client ?nonce:None
end

(** Create a websocket interface with request parameters *)
module Make(Channel:CHANNEL) =
struct
  include Impl(Channel)
  let client ~nonce = client ~nonce
end
