(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Async

val uri_token_arg : (Uri.t * string) Command.Arg_type.t
val ovh_logs : (Uri.t * string) option Command.Param.t

val udp_reporter :
  ?defs:Rfc5424.Tag.tydef list ->
  ?logs:(Uri.t * string) ->
  ?metrics:(Uri.t * string) ->
  unit -> Logs.reporter Deferred.t
(** [udp_reporter ~uri ~token] is a reporter that writes to disk in
    RFC5424 format (syslog) and additionally reports to OVH's Logs Data
    Platform at [uri] with OVH token [token] over UDP. *)

val tcp_tls_reporter :
  ?defs:Rfc5424.Tag.tydef list ->
  ?logs:(Uri.t * string) ->
  ?metrics:(Uri.t * string) ->
  unit -> Logs.reporter Deferred.t
(** [tcp_tls_reporter ~uri ~token] is a reporter that writes to disk in
    RFC5424 format (syslog) and additionally reports to OVH's Logs Data
    Platform at [uri] with OVH token [token] over TCP/SSL. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
