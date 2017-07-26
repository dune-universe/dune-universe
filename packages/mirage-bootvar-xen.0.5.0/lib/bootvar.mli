(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
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

(** Read boot parameter line and store in assoc list.
    Expected format is ["key1=val1 key2=val2"]. *)
val create : unit -> t Lwt.t

(** Get boot parameter. Returns [None] if the parameter is not found. *)
val get : t -> string -> string option

(** Get boot parameter. Raises [Parameter_not_found s] if the parameter is not found. *)
val get_exn : t -> string -> string

exception Parameter_not_found of string

(** Returns the assoc list of key and values. *)
val parameters : t -> (string * string) list

(** Return an argv-like structure. *)
val argv : ?filter:((string * string) -> bool) -> unit -> string array Lwt.t
