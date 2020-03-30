(*
 * Copyright 2003-2016 Savonet team
 *
 * This file is part of Ocaml-cry.
 *
 * Ocaml-cry is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-cry is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-cry; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** OCaml low level implementation of the shout source protocol. *)

type error =
  | Create of exn
  | Connect of exn
  | Close of exn
  | Write of exn
  | Read of exn
  | Busy
  | Ssl_unavailable
  | Not_connected
  | Invalid_usage
  | Unknown_host of string
  | Bad_answer of string option
  | Http_answer of int * string * string

exception Error of error
exception Timeout

type operation = [ `Read | `Write | `Both ]

type transport = {
  write : Bytes.t -> int -> int -> int;
  read : Bytes.t -> int -> int -> int;
  wait_for : operation -> float -> bool;
  close : unit -> unit;
}
