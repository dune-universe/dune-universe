(*
 * Copyright (c) 2013 Citrix Systems, Inc
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
 *)
open Lwt

module type IO_PAGE = sig
  type t

  val to_cstruct : t -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  val get_order : int -> t
  val to_pages : t -> t list
end

let make_counter () =
  let counter = ref 0 in
  fun () ->
    let result = !counter in
    incr counter;
    result

module Io_page = struct
  type file = {
    address: int; (* shared identifier which maps onto filenames *)
    offset: int;  (* offset of this Io_page within the file *)
    length: int;  (* length of this Io_page within the file *)
  }

  type t = {
    buf: (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
    file: file;
  }

  let to_cstruct t = t.buf

  let page_size = 4096

  let next_address = make_counter ()

  let get_order order =
    let rec pow2 = function
      | 0 -> 1
      | n -> n * (pow2 (n - 1)) in
    let address = next_address () in
    let length = page_size * (pow2 order) in
    let name = Printf.sprintf "page.%d" address in
    let fd = Unix.openfile name [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR ] 0o0644 in
    try
      let buf = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout true length in
      let offset = 0 in
      let file = { address; offset; length } in
      { buf; file }
    with e ->
      Unix.close fd;
      raise e

  let to_pages t =
    (* XXX: move to shared library *)
    assert(t.file.length mod page_size = 0);
    let rec loop offset acc =
      if offset < t.file.length
      then
        let length = page_size in
        let address = t.file.address in
        let buf = Bigarray.Array1.sub t.buf offset length in
        let file = { address; offset; length } in
        let t = { buf; file } in
        loop (offset + length) (t :: acc)
      else acc in
    List.rev (loop 0 [])

end

module type GNTTAB = sig
  type handle

  type r
  type h
  type perm = RO | RW

  val get_n : int -> r list Lwt.t
  val to_string : r -> string
  val to_int32 : r -> int32

  val grant_access : domid:int -> perm:perm -> r -> Io_page.t -> unit
  val with_ref: (r -> 'a Lwt.t) -> 'a Lwt.t
  val with_grant : domid:int -> perm:perm -> r -> Io_page.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  val with_grants : domid:int -> perm:perm -> r list -> Io_page.t list -> (unit -> 'a Lwt.t) -> 'a Lwt.t

end

module Gnttab = struct

  type r = int
  type h = unit
  type handle = unit
  type perm = RO | RW

  let next_ref = make_counter ()

  let get_n n =
    let rec loop = function
      | 0 -> []
      | n -> next_ref () :: (loop (n - 1)) in
    return (loop n)

  let to_string = string_of_int
  let to_int32 = Int32.of_int

  let grant_access ~domid ~perm gnt page = failwith "grant_access"
  let with_ref r = failwith "with_ref"
  let with_grant ~domid ~perm gnt page = failwith "with_grant"
  let with_grants ~domid ~perm gnts pages = failwith "with_grants"
end


module type EVENTCHN = sig
  type t

  type handle

  val init: unit -> handle
  val unbind: handle -> t -> unit
  val notify: handle -> t -> unit
  val to_int: t -> int
  val alloc_unbound_port: handle -> int -> t
  val bind_interdomain: handle -> int -> int -> t 

end

module Eventchn = struct

  type t = int option * Unix.file_descr

  let to_int = function
    | None, _ -> -1
    | Some x, _ -> x

  type handle = {
    mutable ports: t list;
  }

  let init () = {
    ports = []
  }

  let unbind h t =
    h.ports <- List.filter (fun t' -> t' <> t) h.ports

  let notify _ (_, fd) =
    let _ = Unix.write fd "!" 0 1 in
    ()

  let path_of_port = Printf.sprintf "eventchn.%d"

  let free_port =
    let counter = ref 0 in
    fun () ->
      let result = !counter in
      incr counter;
      result

  let bind_interdomain h domid port =
    let path = path_of_port port in
    let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let () = Unix.connect sock (Unix.ADDR_UNIX path) in
    h.ports <- (None, sock) :: h.ports;
    (None, sock)

  let alloc_unbound_port h domid =
    let port = free_port () in
    let path = path_of_port port in
    let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.bind sock (Unix.ADDR_UNIX path);
    Unix.listen sock 5;
    let (fd, _) = Unix.accept sock in
    h.ports <- (Some port, fd) :: h.ports;
    (Some port, fd)

  let pending h = failwith "not implemented"
end


module Io_page' = (Io_page : IO_PAGE)
module Gnttab' = (Gnttab : GNTTAB)
module Eventchn'  = (Eventchn : EVENTCHN)
