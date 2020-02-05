(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Tag : sig
  open Logs.Tag

  type _ typ =
    | String : string typ
    | Bool : bool typ
    | Float : float typ
    | I64 : int64 typ
    | U64 : Uint64.t typ
    | U : unit typ
    (** Type of a metric type (compatible with Warp10 and flowgger
        representation). *)

  type tydef = Dyn : 'a typ * 'a def -> tydef
  (** Existential type for log tag definiton. *)

  module TS : Set.S with type elt := tydef

  val string : string def -> tydef
  val bool : bool def -> tydef
  val float : float def -> tydef
  val i64 : int64 def -> tydef
  val u64 : Uint64.t def -> tydef
  val u : unit def -> tydef

  val def : 'a typ -> tydef -> 'a def option
  (** [def typ tydef] is [Some def] is tydef contains a def of type
      ['a], or [None] otherwise. *)

  val add : 'a typ -> tydef -> 'a -> set -> set
  (** [add is like Logs.Tag.add but uses [tydef] instead of def]. *)

  val find : 'a typ -> tydef -> set -> ('a def * 'a option) option
  (** [find typ tydef set] is [Some value] of requested type and
      definition if found in [set], or [None] otherwise. *)
end

type t = {
  header : header ;
  structured_data : sd_element list ;
  msg : [`Utf8 of string | `Ascii of string] ;
}

and sd_element = {
  section: string ;
  defs : Tag.tydef list ;
  tags : Logs.Tag.set ;
}

and header = {
  facility : Syslog_message.facility ;
  severity : Syslog_message.severity ;
  version : int ;
  ts : Ptime.t ;
  tz_offset_s : int option ;
  hostname : string ;
  app_name : string ;
  procid : string ;
  msgid : string ;
}

val create_sd_element :
  ?defs:Tag.tydef list -> section:string ->
  tags:Logs.Tag.set -> sd_element

val create :
  ?facility:Syslog_message.facility ->
  ?severity:Syslog_message.severity ->
  ?ts:Ptime.t ->
  ?tz_offset_s:int ->
  ?hostname:string ->
  ?app_name:string ->
  ?procid:string ->
  ?msgid:string ->
  ?structured_data:sd_element list ->
  ?msg:[`Utf8 of string | `Ascii of string] -> unit -> t

val fcreate :
  ?facility:Syslog_message.facility ->
  ?severity:Syslog_message.severity ->
  ?ts:Ptime.t ->
  ?tz_offset_s:int ->
  ?hostname:string ->
  ?app_name:string ->
  ?procid:string ->
  ?msgid:string ->
  ?structured_data:sd_element list ->
  unit -> ('a, Format.formatter, unit, t) format4 -> 'a

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val show : t -> string
val of_string : string -> (t, t Tyre.error) result

val severity_of_level : Logs.level -> Syslog_message.severity

(**/**)

val equal_structured_data :
  sd_element list -> sd_element list -> bool
val pp_print_structured_data :
  Format.formatter -> sd_element list -> unit
val sd_name : string Tyre.t
val structured_data : sd_element list Tyre.t

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
