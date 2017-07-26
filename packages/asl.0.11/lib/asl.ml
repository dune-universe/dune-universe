(*
 * Copyright (c) 2015 Unikernel Systems
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type level = [
  | `Emerg
  | `Alert
  | `Crit
  | `Err
  | `Warning
  | `Notice
  | `Info
  | `Debug
]

external get_asl_level_EMERG: unit -> int = "stub_get_asl_level_EMERG"
external get_asl_level_ALERT: unit -> int = "stub_get_asl_level_ALERT"
external get_asl_level_CRIT: unit -> int = "stub_get_asl_level_CRIT"
external get_asl_level_ERR: unit -> int = "stub_get_asl_level_ERR"
external get_asl_level_WARNING: unit -> int = "stub_get_asl_level_WARNING"
external get_asl_level_NOTICE: unit -> int = "stub_get_asl_level_NOTICE"
external get_asl_level_INFO: unit -> int = "stub_get_asl_level_INFO"
external get_asl_level_DEBUG: unit -> int = "stub_get_asl_level_DEBUG"

let asl_level_EMERG = get_asl_level_EMERG()
let asl_level_ALERT = get_asl_level_ALERT()
let asl_level_CRIT = get_asl_level_CRIT()
let asl_level_ERR = get_asl_level_ERR()
let asl_level_WARNING = get_asl_level_WARNING()
let asl_level_NOTICE = get_asl_level_NOTICE()
let asl_level_INFO = get_asl_level_INFO()
let asl_level_DEBUG = get_asl_level_DEBUG()

let int_of_level = function
  | `Emerg -> asl_level_EMERG
  | `Alert -> asl_level_ALERT
  | `Crit -> asl_level_CRIT
  | `Err -> asl_level_ERR
  | `Warning -> asl_level_WARNING
  | `Notice -> asl_level_NOTICE
  | `Info -> asl_level_INFO
  | `Debug -> asl_level_DEBUG

module Client = struct
  type t

  type opt = [
    | `Stderr
    | `No_delay
    | `No_remote
  ]

  external asl_open: string -> string -> bool -> bool -> bool -> t = "stub_asl_open"

  external asl_open_null: unit -> t = "stub_asl_open_null"

  let null = lazy (asl_open_null ())

  let create ~ident ~facility ?(opts=[]) () =
    let stderr = List.mem `Stderr opts in
    let no_delay = List.mem `No_delay opts in
    let no_remote = List.mem `No_remote opts in
    asl_open ident facility stderr no_delay no_remote

  external asl_add_output_file: t -> Unix.file_descr -> string -> string
    -> int -> bool = "stub_asl_add_output_file"

  let add_output_file t fd msg_fmt time_fmt level_up_to =
    asl_add_output_file t fd msg_fmt time_fmt (int_of_level level_up_to)
end

module Opt = struct
  let iter f = function None -> () | Some x -> f x
end

module Message = struct

  type t

  type ty = [
    | `Msg
  ]

  external asl_new_msg: unit -> t = "stub_asl_new_msg"

  external asl_set: t -> string -> string -> unit = "stub_asl_set"

  external asl_set_TIME: t -> string -> unit = "stub_asl_set_TIME"
  external asl_set_HOST: t -> string -> unit = "stub_asl_set_HOST"
  external asl_set_SENDER: t -> string -> unit = "stub_asl_set_SENDER"
  external asl_set_FACILITY: t -> string -> unit = "stub_asl_set_FACILITY"
  external asl_set_PID: t -> string -> unit = "stub_asl_set_PID"
  external asl_set_UID: t -> string -> unit = "stub_asl_set_UID"
  external asl_set_GID: t -> string -> unit = "stub_asl_set_GID"
  external asl_set_LEVEL: t -> string -> unit = "stub_asl_set_LEVEL"
  external asl_set_MSG: t -> string -> unit = "stub_asl_set_MSG"

  let create ?ty:_ ?time ?host ?sender ?facility ?pid ?uid
  ?gid ?level ?msg ?(extra=[]) () =
    let m = asl_new_msg () in
    Opt.iter (asl_set_TIME m) time;
    Opt.iter (asl_set_HOST m) host;
    Opt.iter (asl_set_SENDER m) sender;
    Opt.iter (asl_set_FACILITY m) facility;
    Opt.iter (asl_set_PID m) pid;
    Opt.iter (asl_set_UID m) uid;
    Opt.iter (asl_set_GID m) gid;
    Opt.iter (asl_set_LEVEL m) level;
    Opt.iter (asl_set_MSG m) msg;
    List.iter (fun (k, v) -> asl_set m k v) extra;
    m
end


external asl_log: Client.t -> Message.t -> int -> string -> unit = "stub_asl_log"

let log ?client message level body =
  let client = match client with
    | Some c -> c
    | None -> Lazy.force Client.null in
  let level = int_of_level level in
  asl_log client message level body
