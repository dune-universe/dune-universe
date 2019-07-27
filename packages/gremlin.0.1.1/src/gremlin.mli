module Websocket : sig
  val new_connection :
    Uri.t ->
    ((unit -> Websocket.Frame.t Lwt.t) * (Websocket.Frame.t -> unit Lwt.t))
    Lwt.t
  type op_processor = Session of string | Standard
  type message_status =
      Response_invalid of response_invalid
    | Request_failed of request_failed
    | Unmatched_request_id
    | Good of Yojson.Basic.t
  and request_failed = Non_200_status_code of Yojson.Basic.t
  and response_invalid =
      No_status_field
    | Missing_fields
    | Missing_status_fields
    | Missing_field of string
    | Invalid_status_json_type of Yojson.Basic.t
    | Invalid_result_json_type of Yojson.Basic.t
    | Invalid_result_field
    | Invalid_request_id_json_type of Yojson.Basic.t
    | Result_fields_wrong
    | Json_parse_failure
    | Unknown_json_parse_failure
  val print_message_status : message_status -> unit
  val run_query :
    ((unit -> Websocket.Frame.t Lwt.t) * (Websocket.Frame.t -> unit Lwt.t))
    Lwt.t -> string -> (Yojson.Basic.t, message_status) result Lwt.t
  val run_queries_transaction :
    ((unit -> Websocket.Frame.t Lwt.t) * (Websocket.Frame.t -> unit Lwt.t))
    Lwt.t -> string list -> (Yojson.Basic.t, message_status) result Lwt.t
end
