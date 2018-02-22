
(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

(** {2 Prelude} *)

type 'a or_error = ('a, string) Result.result

type mime_data = {
  mime_type: string;
  mime_content: string; (* raw content *)
  mime_b64: bool; (* if true, content will be encoded with base64 *)
}

(* list of mime objects, all of distinct types *)
type mime_data_bundle = mime_data list

(** {2 User-Defined Kernel} *)
module Kernel : sig
  type exec_action =
    | Mime of mime_data_bundle

  (* TODO:
     make the [exec] return type be asynchronous
     - return [Ok str] as a future
     - return a stream of actions
  *)

  type exec_status_ok = {
    msg: string option;
    (* main message *)
    actions: exec_action list;
    (* other actions *)
  }

  type completion_status = {
    completion_matches: string list;
    completion_start: int;
    completion_end: int;
  }

  type is_complete_reply =
    | Is_complete
    | Is_not_complete of string (* indent *)

  type history_request = Protocol_j.history_request

  type inspect_request = Protocol_j.inspect_request = {
    ir_code: string;
    ir_cursor_pos: int; (* cursor pos *)
    ir_detail_level: int; (* 0 or 1 *)
  }

  type inspect_reply_ok = {
    iro_status: string; (* "ok" or "error" *)
    iro_found: bool;
    iro_data: mime_data_bundle;
  }

  val mime : ?base64:bool -> ty:string -> string -> exec_action

  val ok : ?actions:exec_action list -> string option -> exec_status_ok

  type t = {
    init: unit -> unit Lwt.t;
    exec: count:int -> string -> exec_status_ok or_error Lwt.t; (* TODO: user expressions *)
    is_complete: string -> is_complete_reply Lwt.t;
    language: string;
    language_version: int list;
    banner: string option; (* displayed at startup *)
    file_extension: string;
    mime_type: string option; (* default: text/plain *)
    complete: pos:int -> string -> completion_status Lwt.t;
    inspect: inspect_request -> inspect_reply_ok or_error Lwt.t;
    history: history_request -> string list Lwt.t;
  }

  val make :
    ?banner:string ->
    ?file_extension:string ->
    ?mime_type:string ->
    ?init:(unit -> unit Lwt.t) ->
    ?is_complete:(string -> is_complete_reply Lwt.t) ->
    ?complete:(pos:int -> string -> completion_status Lwt.t) ->
    ?inspect: (inspect_request -> inspect_reply_ok or_error Lwt.t) ->
    ?history:(history_request -> string list Lwt.t) ->
    language_version:int list ->
    language:string ->
    exec:(count:int -> string -> exec_status_ok or_error Lwt.t) ->
    unit ->
    t
end

type t

val make : ?key:string -> Sockets.t -> Kernel.t -> t

type run_result =
  | Run_stop
  | Run_restart

val run : t -> run_result Lwt.t
