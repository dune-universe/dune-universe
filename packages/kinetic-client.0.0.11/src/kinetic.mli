(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
*)

module Config: sig
  type t = {
      vendor: string;
      model:string;
      serial_number: bytes;
      world_wide_name: bytes;
      version: string;
      ipv4_addresses: bytes list;
      (* limits *)
      max_key_size:int;
      max_value_size: int;
      max_version_size:int;
      max_tag_size: int;
      max_connections: int;
      max_outstanding_read_requests: int;
      max_outstanding_write_requests: int;
      max_message_size: int;
      max_key_range_count: int;
      (* are in protocol definition but device doesn't send them *)
      max_operation_count_per_batch: int option;
      (* max_batch_count_per_device: int; *)
      max_batch_size: int option;
      max_deletes_per_batch : int option;
      timeout : float;

    }
  (*

    val make : vendor:string ->
               model:string ->
               serial_number:string ->
               world_wide_name:string ->
               version: string -> t
   *)
  val show : t -> string
end

type off = int
type len = int
type 'a slice = 'a * off * len

type key = bytes
type version = bytes option
type timeout_ms = int64

type priority =
  | Normal
  | Lowest
  | Lower
  | Low
  | Lowernormal
  | Highernormal
  | High
  | Higher
  | Highest

module Tag : sig
  type t =
    | Invalid of Bytes.t
    | Sha1 of Bytes.t
    | Crc32 of int32
end

module type INTEGRATION = sig
  type value

  type socket 
  val create : int -> value
  val show : value -> string
  val show_socket : socket -> string

  (*  the value type can be chosen by the integrator ... *)

  val read  : socket -> value -> off -> len -> int Lwt.t
  val write : socket -> value -> off -> len -> int Lwt.t

  (* but, the protocol uses bytes because of piqi *)
  val read_bytes  : socket -> Bytes.t -> off -> len -> int Lwt.t
  val write_bytes : socket -> Bytes.t -> off -> len -> int Lwt.t

  val make_sha1  : value -> off -> len -> Tag.t
  val make_crc32 : value -> off -> len -> Tag.t

end

module BytesIntegration : sig
  type value = Bytes.t
  type socket = Lwt_ssl.socket
  val create : int -> value
  val show : value -> string
  val show_socket : socket -> string
  val read  : socket -> value -> off -> len -> int Lwt.t
  val write : socket -> value -> off -> len -> int Lwt.t

  val read_bytes : socket -> Bytes.t -> off -> len -> int Lwt.t
  val write_bytes : socket -> Bytes.t -> off -> len -> int Lwt.t
    
  val make_sha1  : value -> off -> len -> Tag.t
  val make_crc32 : value -> off -> len -> Tag.t
end

module Error : sig
  type msg = string
  type t =
    | KineticError of int * msg
    | Generic of string * int * msg (* file, line, msg *)
    | Timeout of float * msg        (* delta_t, msg *)
    | Assert of msg 

  val show : t -> string
end
module Make(I:INTEGRATION) : sig

  module Entry : sig
    type t = {
        key: key;
        db_version: version;
        new_version: version;
        vt : (I.value slice * Tag.t )option;
      }

    val make :
      key:key ->
      db_version:version ->
      new_version:version ->
      (I.value slice * Tag.t) option ->
      t
      
    val show: t -> string
  end
       
  type session

  val get_connection_id : session -> int64

  type batch

  val get_batch_id : batch -> int32

  type client

  val get_session : client -> session

  type synchronization =
    | WRITETHROUGH
    | WRITEBACK
    | FLUSH


  val get_config : session -> Config.t

  val make_sha1  : I.value slice -> Tag.t
  val make_crc32 : I.value slice -> Tag.t

  type 'a result = ('a, Error.t) Lwt_result.t

  val handshake : string -> int64 -> ?trace:bool -> ?timeout:float -> ?max_operation_count_per_batch:int 
                  -> I.socket -> session result
  (** The initial contact with the device, which
        will send some information that is needed in the session
   *)
  
  val tracing : session -> bool -> unit
  (** turn on or of the trace logging of the raw messages

   *)

  val put:
    ?timeout:timeout_ms -> ?priority:priority -> client ->
    key -> I.value slice
    -> db_version:version
    -> new_version:version
    -> forced:bool option
    -> synchronization: synchronization option
    -> tag: Tag.t option
    -> unit result
  (** insert a key value pair.
        db_version is the version that's supposed to be the current version
        in the database.
        new_version is the version of the key value pair _after_ the update.
        forced updates happen regardless the db_version
   *)

  val delete_forced:
    ?timeout:timeout_ms -> ?priority:priority -> client ->
    key -> unit result

  val get :
    ?timeout:timeout_ms -> ?priority:priority -> client ->
    key -> (I.value * version) option result

  val noop: ?timeout:timeout_ms -> ?priority:priority -> client -> unit result

  val instant_secure_erase: ?pin:bytes -> client -> unit result

  val download_firmware: client -> I.value slice -> unit result

  val get_key_range:
    ?timeout:timeout_ms -> ?priority:priority -> client ->
    key -> bool ->
    (key * bool) option ->
    bool -> int ->
    key list result

  
  val get_capacities : ?timeout:timeout_ms -> ?priority:priority -> client -> (int64 * float) result
  (** returns capacity of the drive and portion full
   *)

  type forced = bool option

  type batch_operation =
    | BPut of Entry.t * forced
    | BDel of Entry.t * forced

  val do_batch :
    ?timeout:timeout_ms -> ?priority:priority -> client -> batch_operation list -> unit result

  (* (* we might need it again in the future *)
    val p2p_push : session -> connection ->
                   (string * int * bool) ->
                   (string * string option) list ->
                   unit Lwt.t
   *)
    

  val wrap_socket :
    ?trace:bool ->
    ?timeout:float ->
    ?secret:string ->
    ?cluster_version:int64 ->
    ?max_operation_count_per_batch:int ->
    I.socket ->
    (unit -> unit Lwt.t)
    -> client result
    
  val dispose : client -> unit Lwt.t

end
