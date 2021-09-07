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

(** Low-level functions to write to the Windows event log.

The Windows event log is similar to syslog on Unix systems. The main difference
is that the log records "events" which have a "category", an "event id" and
an array of "insertion strings". Applications are expected to enumerate their
categories and event ids and use the message compiler (mc.exe) to create a
resource which is linked into the application. The event log viewer (or other
clients) look up the source name in the registry, read the resources and decode
the event data.

By default if no resources can be found describing the specific categories and
events of the application, then the event log viewer will show the categories
and events as integers and the insertion strings as plain text. This style of
usage is similar to syslog on a Unix system.

Low-level example:

{[
  let log = Eventlog.register "Mirage.exe" in
  let category = 0 and event = 1 in
  Eventlog.report log `Success category event [|
    "insertion string 1";
    "insertion string 2";
  |]
]}

You may wish to use the high-level {!Log} reporter interface instead:
{[
  let log = Eventlog.register "Mirage.exe" in
  Logs.set_reporter (Log_eventlog.reporter log ());
  Log.err (fun f -> f "This is an error");
  Log.info (fun f -> f "This is informational");
  Log.debug (fun f -> f "This is lowly debugging data");
]}


For more context, please read the following documents:
{ol
{li
{{:https://msdn.microsoft.com/en-us/library/aa363679(v=vs.85).aspx}
ReportEvent API documentation}
}
{li
{{:https://support.microsoft.com/en-us/kb/166902}
HOWTO: Troubleshooting the "Event Message Not Found" message}
}
}
*)

type t
(** An event log handle, see {!register}. *)

val register : ?server:string -> string -> t
(** [register server source] registers the source named [source] with the
    event log on server [server]. If [server] is [None] then the local event
    log is used. *)

type ty =
  [ `Success
  | `Audit_failure
  | `Audit_success
  | `Error
  | `Information
  | `Warning ]
(** Type of event to be logged. *)

val string_of_ty : ty -> string

val report : t -> ty -> int -> int -> string array -> unit
(** [report t ty category event strings] reports an event to the log [t]. The
    event has a global "type", as well as source-specific category and event ids.
    Each event takes an array of "insertion strings" -- the system log viewer
    will look up the category, event and user's language setting in the resource
    (.exe or .dll) associated with the event source in the registrty, and then
    "insert" the strings as parameters inside the template. *)
