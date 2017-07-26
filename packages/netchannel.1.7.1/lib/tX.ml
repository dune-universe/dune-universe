(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2015 Citrix Inc
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
open Result

module Request = struct
  type error = { impossible : 'a. 'a }

  type t = {
    gref: int32;
    offset: int;
    flags: Flags.t;
    id: int;

    (** For frames split over multiple requests, first.size is the total
        size of the frame. Each of the following requests gives the size
        of that fragment. The receiver recovers the actual size of the
        first fragment by subtracting all of the other sizes. *)
    size: int;
  }

  [%%cstruct
  type req = {
    gref: uint32_t;
    offset: uint16_t;
    flags: uint16_t;
    id: uint16_t;
    size: uint16_t;
  } [@@little_endian]
  ]

  let write t slot =
    let flags = Flags.to_int t.flags in
    set_req_gref slot t.gref;
    set_req_offset slot t.offset;
    set_req_flags slot flags;
    set_req_id slot t.id;
    set_req_size slot t.size

  let within_page name x =
    if x < 0 || x > 4096
    then Error (Printf.sprintf "%s is corrupt: expected 0 <= %s <= 4096 but got %d" name name x)
    else Ok x

  let read slot =
    let open ResultM in
    let gref = get_req_gref slot in
    let offset = get_req_offset slot in
    within_page "TX.Request.offset" offset
    >>= fun offset ->
    let flags = Flags.of_int (get_req_flags slot) in
    let id = get_req_id slot in
    let size = get_req_size slot in
    return { gref; offset; flags; id; size }

  let flags t = t.flags

  let size t = Ok t.size
end

module Response = struct
  [%%cenum
  type status =
    | DROPPED [@id 0xfffe]
    | ERROR   [@id 0xffff]
    | OKAY    [@id 0]
    | NULL    [@id 1]
    [@@int16_t]
  ]
  type t = {
    id: int;
    status: status;
  }

  [%%cstruct
  type resp = {
    id: uint16_t;
    status: uint16_t;
  } [@@little_endian]
  ]
  let write t slot =
    set_resp_id slot t.id;
    set_resp_status slot (status_to_int t.status)

  let read slot =
    let id = get_resp_id slot in
    let st = get_resp_status slot in
    match int_to_status st with
    | None -> failwith (Printf.sprintf "Invalid TX.Response.status %d" st)
    | Some status -> { id; status }
end

let total_size = max Request.sizeof_req Response.sizeof_resp
let () = assert(total_size = 12)
