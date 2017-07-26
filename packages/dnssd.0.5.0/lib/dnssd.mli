(**
 * Copyright (C) 2017 Docker Inc <dave.scott@docker.com>
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

(** Bindings to the macOS DNS SD API

    These functions allow DNS queries to be made via the macOS resolver.

    @see <https://opensource.apple.com/source/mDNSResponder/mDNSResponder-320.5/mDNSShared/dns_sd.h>dns_sd.h

*)

val is_supported_on_this_platform: unit -> bool
(** [is_supported_on_this_platform ()] is [true] if this library is supported
    on this platform. On other platforms the APIs may throw runtime failures. *)

type error =
  | Unknown
  | NoSuchName
  | NoMemory
  | BadParam
  | BadReference
  | BadState
  | BadFlags
  | Unsupported
  | NotInitialized
  | AlreadyRegistered
  | NameConflict
  | Invalid
  | Firewall
  | Incompatible
  | BadInterfaceIndex
  | Refused
  | NoSuchRecord
  | NoAuth
  | NoSuchKey
  | NATTraversal
  | DoubleNAT
  | BadTime
  | BadSig
  | BadKey
  | Transient
  | ServiceNotRunning
  | NATPortMappingUnsupported
  | NATPortMappingDisabled
  | NoRouter
  | PollingMode
  | Timeout
(** Possible errors from [query] *)

val string_of_error: error -> string

val query: string -> Dns.Packet.q_type -> (Dns.Packet.rr list, error) result
(** [query name ty] returns a list of resource records of type [ty] bound to
    [name] *)

module LowLevel: sig
  (** A low-level interface which exposes the Unix domain socket used to talk
      to the daemon. *)

  type query

  val query: string -> Dns.Packet.q_type -> query
  (** [query name type] creates a query for [name] and [type]. This call does
      not block. *)

  exception Cancelled
  (** Raised if an operation is called on a cancelled query *)

  val socket: query -> Unix.file_descr
  (** [socket query] returns the underlying Unix domain socket suitable for
      [select()] [kqueue] etc. When the socket is readable, [response] can be
      called without blocking (very much).
      This raises [Cancelled] if the query has been cancelled. *)

  val response: query -> (Dns.Packet.rr list, error) result
  (** [response query] reads the responses which have arrived for [query].
      This function will block unless the caller has waited for events on the
      Unix domain socket.
      This raises [Cancelled] if the query has been cancelled. *)

  val cancel: query -> unit
  (** [cancel query] causes an outstanding query to be cancelled and resources
      freed *)
end
