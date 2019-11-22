open! Core

(* https://tools.ietf.org/html/rfc6455#section-7.4 *)

module T = struct
  type t =
    | Normal_closure
    | Endpoint_going_away
    | Protocol_error
    | Cannot_accept_data
    | Reserved_0
    | No_status_code
    | Closed_abnormally
    | Invalid_message_sent
    | Policy_violation
    | Message_too_large
    | Invalid_handshake
    | Unexpected_condition
    | Tls_handshake_failure
  [@@deriving sexp, compare, enumerate]
end

include T
include Comparable.Make (T)

let to_int = function
  | Normal_closure -> 1000
  | Endpoint_going_away -> 1001
  | Protocol_error -> 1002
  | Cannot_accept_data -> 1003
  | Reserved_0 -> 1004
  | No_status_code -> 1005
  | Closed_abnormally -> 1006
  | Invalid_message_sent -> 1007
  | Policy_violation -> 1008
  | Message_too_large -> 1009
  | Invalid_handshake -> 1010
  | Unexpected_condition -> 1011
  | Tls_handshake_failure -> 1015
;;

let of_int_map = List.map all ~f:(fun t -> to_int t, t) |> Int.Map.of_alist_exn

let of_int code =
  match Core.Map.find of_int_map code with
  | Some t -> Ok t
  | None -> error_s [%message "Unknown close code" (code : int)]
;;
