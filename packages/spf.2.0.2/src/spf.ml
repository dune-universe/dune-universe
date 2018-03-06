type server

type dns = Dns_resolv
         | Dns_cache
         | Dns_zone

type req

type request =
  { request : req
  ; server  : server
  }

type comments =
  { explanation  : string
  ; smtp_comment : string
  }

type result
  = Invalid
  | Neutral of comments
  | Pass
  | Fail of comments
  | Softfail of comments
  | None
  | Temperror
  | Permerror

type reason
  = No_reason
  | Failure
  | Localhost
  | Local_policy
  | Mech
  | Default
  | Secondary_mx

type response =
  { result             : result
  ; reason             : reason
  ; received_spf       : string
  ; received_spf_value : string
  ; header_comment     : string
  }

exception Error of string

let _ = Callback.register_exception "Error" (Error "")

external server : ?debug:bool -> dns -> server = "caml_spf_server_new"
external free_server : server -> unit = "caml_spf_server_free"

external spf_request_new : server -> req = "caml_spf_request_new"
external spf_free_request : req -> unit = "caml_spf_request_free"

external request_set_inet_addr : req -> Unix.inet_addr -> unit =
  "caml_spf_request_set_inet_addr"

external request_set_ipv4_str : req -> string -> unit =
  "caml_spf_request_set_ipv4_str"

external request_set_ipv6_str : req -> string -> unit =
  "caml_spf_request_set_ipv6_str"

external request_set_helo_domain : req -> string -> unit =
  "caml_spf_request_set_helo_dom"

external request_set_envelope_from : req -> string -> unit =
  "caml_spf_request_set_env_from"

external spf_query_mailfrom : req -> response =
  "caml_spf_request_query_mailfrom"

let request server =
  let req = spf_request_new server in
  { request = req; server = server }

let free_request req =
  spf_free_request req.request

let set_inet_addr req ip =
  request_set_inet_addr req.request ip

let set_ipv4_str req ip =
  request_set_ipv4_str req.request ip

let set_ipv6_str req ip =
  request_set_ipv6_str req.request ip

let set_helo_domain req domain =
  request_set_helo_domain req.request domain

let set_envelope_from req from =
  request_set_envelope_from req.request from

let finalize f g x =
  try let r = f x in g x; r
  with e -> g x; raise e

let query_mailfrom req =
  spf_query_mailfrom req.request

let check_helo server client_addr helo =
  let req = request server in
  let check req =
    set_inet_addr req client_addr;
    set_helo_domain req helo;
    query_mailfrom req in
  let close req =
    free_request req in
  finalize check close req

let check_from server client_addr from =
  if not (String.contains from '@') then
    raise (Error ("invalid from address: " ^ from));
  let req = request server in
  let check req =
    set_inet_addr req client_addr;
    set_envelope_from req from;
    query_mailfrom req in
  let close req =
    free_request req in
  finalize check close req

let string_of_result = function
  | Invalid -> "(invalid)"
  | Neutral _ -> "neutral"
  | Pass -> "pass"
  | Fail _ -> "fail"
  | Softfail _ -> "softfail"
  | None -> "none"
  | Temperror -> "temperror"
  | Permerror -> "permerror"

external string_of_reason : reason -> string =
  "caml_spf_strreason"

let result r = r.result
let reason r = r.reason
let received_spf r = r.received_spf
let received_spf_value r = r.received_spf_value
let header_comment r = r.header_comment

let smtp_comment c =
  c.smtp_comment

let explanation c =
  c.explanation
