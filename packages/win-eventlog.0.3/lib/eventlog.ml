(*
 * Copyright (c) 2016 Docker Inc
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

type t

external registerEventSource: string option -> string -> t = "stub_register_event_source"

let register ?server source = registerEventSource server source

type ty = [
  | `Success
  | `Audit_failure
  | `Audit_success
  | `Error
  | `Information
  | `Warning
]

(* https://msdn.microsoft.com/en-gb/library/windows/desktop/aa363679(v=vs.85).aspx *)
let int_of_ty = function
  | `Success       -> 0x0000
  | `Audit_failure -> 0x0010
  | `Audit_success -> 0x0008
  | `Error         -> 0x0001
  | `Information   -> 0x0004
  | `Warning       -> 0x0002

let string_of_ty = function
  | `Success       -> "EVENTLOG_SUCCESS"
  | `Audit_failure -> "EVENTLOG_AUDIT_FAILURE"
  | `Audit_success -> "EVENTLOG_AUDIT_SUCCESS"
  | `Error         -> "EVENTLOG_ERROR_TYPE"
  | `Information   -> "EVENTLOG_INFORMATION_TYPE"
  | `Warning       -> "EVENTLOG_WARNING_TYPE"

external reportEvent: t -> int -> int -> int -> string array -> unit = "stub_report_event"

let report t ty category event strings =
  reportEvent t (int_of_ty ty) category event strings
