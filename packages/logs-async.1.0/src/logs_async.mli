(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   logs-async 1.0
  ---------------------------------------------------------------------------*)

(** {!Async} logging.

    The log functions of this module return [Async] threads that proceed
    only when the log operation is over, as defined by the current
    {!Logs.reporter}.

    See a {{!report_ex}cooperative reporter example}.

    {e 1.0 - {{:https://github.com/vbmithr/logs-async }homepage}} *)

(** {1 Log functions} *)

open Async_kernel
open Result

type 'a log = ('a, unit Deferred.t) Logs.msgf -> unit Deferred.t
(** The type for Async log functions. The returned thread only proceeds
    once the log operation is over. See {!Logs.log}. *)

val msg : ?src:Logs.src -> Logs.level -> 'a log
(** See {!Logs.msg}. *)

val app : ?src:Logs.src -> 'a log
(** See {!Logs.app}. *)

val err : ?src:Logs.src -> 'a log
(** See {!Logs.err}. *)

val warn : ?src:Logs.src -> 'a log
(** See {!Logs.warn}. *)

val info : ?src:Logs.src -> 'a log
(** See {!Logs.info}. *)

val debug : ?src:Logs.src -> 'a log
(** See {!Logs.debug}. *)

val kmsg : (unit -> 'b Deferred.t) -> ?src:Logs.src ->
  Logs.level -> ('a, 'b Deferred.t) Logs.msgf -> 'b Deferred.t
(** See {!Logs.kmsg}. *)

(** {2 Logging {!result} value [Error]s} *)

val on_error : ?src:Logs.src -> ?level:Logs.level -> ?header:string ->
  ?tags:Logs.Tag.set -> pp:(Format.formatter -> 'b -> unit) ->
  use:('b -> 'a Deferred.t) -> ('a, 'b) result Deferred.t -> 'a Deferred.t
(** See {!Logs.on_error}. *)

val on_error_msg : ?src:Logs.src -> ?level:Logs.level -> ?header:string ->
  ?tags:Logs.Tag.set -> use:(unit -> 'a Deferred.t) ->
  ('a, [`Msg of string]) result Deferred.t -> 'a Deferred.t
(** See {!Logs.on_error_msg}. *)

(** {1 Source specific log functions} *)

module type LOG = sig
  val msg : Logs.level -> 'a log
  (** See {!Logs.msg}. *)

  val app : 'a log
  (** See {!Logs.app}. *)

  val err : 'a log
  (** See {!Logs.err}. *)

  val warn : 'a log
  (** See {!Logs.warn}. *)

  val info : 'a log
  (** See {!Logs.info}. *)

  val debug : 'a log
  (** See {!Logs.debug}. *)

  val kmsg : ?over:(unit -> unit) -> (unit -> 'b Deferred.t) ->
    Logs.level -> ('a, 'b Deferred.t) Logs.msgf -> 'b Deferred.t
  (** See {!Logs.kmsg}. *)

  (** {2 Logging {!result} value [Error]s} *)

  val on_error : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> pp:(Format.formatter -> 'b -> unit) ->
    use:('b -> 'a Deferred.t) -> ('a, 'b) result Deferred.t -> 'a Deferred.t
  (** See {!Logs.on_error}. *)

  val on_error_msg : ?level:Logs.level -> ?header:string ->
    ?tags:Logs.Tag.set -> use:(unit -> 'a Deferred.t) -> ('a, [`Msg of
    string]) result Deferred.t -> 'a Deferred.t
  (** See {!Logs.on_error_msg}. *)
end

val src_log : Logs.src -> (module LOG)
(** [src_log src] is a {{!LOG}set of logging functions} for [src]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff
   Copyright (c) 2015 Daniel C. Bünzli

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
