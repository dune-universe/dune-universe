(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

val host : Errno.Host.t
(** [host] is the bidirectional error number map for the host upon
    which this code is executing. *)

val to_unix : ?host:Errno.Host.t -> Errno.t -> Unix.error option
(** [to_unix ?host errno] is the {!Unix.error} corresponding to
    [errno] if one exists. If [host] is not supplied, {!host} will be
    used. *)

val of_unix : ?host:Errno.Host.t -> Unix.error -> Errno.t list
(** [of_unix ?host error] is the list of symbolic error numbers
    corresponding to the {!Unix.error}, [error]. If [host] is not
    supplied, {!host} will be used. *)

val get_errno : unit -> Signed.sint
(** [get_errno ()] returns the current value of the C [errno]
    thread-local variable. *)

val reset_errno : unit -> unit
(** [reset_errno ()] sets the current value of the C [errno]
    thread-local variable to 0. *)

val raise_errno : ?call:string -> ?label:string -> Signed.sint -> 'a
(** [raise_errno ?call ?label errno] raises {!Errno.Error} after
    converting [errno] to the appropriate variants via {!host}. [call]
    and [label] default to the empty string. *)

val raise_on_errno : ?call:string -> ?label:string -> (unit -> 'a option) -> 'a
(** [raise_on_errno ?call ?label fn] raises {!Errno.Error} using the
    code in the C [errno] variable if [fn] returns [None]. [call] and
    [label] default to the empty string. *)

val to_errno_exn : exn -> exn
(** [to_errno_exn exn] converts [exn] into an {!Errno.Error} if it is
    a {!Unix.Unix_error} and otherwise does not modify it. *)

val with_errno_exn : (unit -> 'a) -> 'a
(** [with_errno_exn fn] raises {!Errno.Error} instead of
    {!Unix.Unix_error} if [fn] raises the latter. *)

val to_unix_exn : exn -> exn
(** [to_unix_exn exn] converts [exn] into a {!Unix.Unix_error} if it
    is an {!Errno.Error} and an errno code exists on the present host.
    Otherwise, [to_unix_exn] does not modify [exn]. *)

val with_unix_exn : (unit -> 'a) -> 'a
(** [with_unix_exn fn] raises {!Unix.Unix_error} instead of
    {!Errno.Error} if [fn] raises the latter and an errno code exists
    on the present host. *)
