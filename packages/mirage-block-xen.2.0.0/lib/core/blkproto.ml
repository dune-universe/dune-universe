(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems Inc
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

let ( >>= ) x f = match x with
  | Error _ as y -> y
  | Ok x -> f x
let list l k =
  if not(List.mem_assoc k l)
  then Error (`Msg (Printf.sprintf "missing %s key" k))
  else Ok (List.assoc k l)
let int x = try Ok (int_of_string x) with _ -> Error (`Msg ("not an int: " ^ x))
let int32 x = try Ok (Int32.of_string x) with _ -> Error (`Msg ("not an int32: " ^ x))

(* Control messages via xenstore *)

module Mode = struct
  type t = ReadOnly | ReadWrite
  let to_string = function
    | ReadOnly -> "r"
    | ReadWrite -> "w"
  let to_int = function
    | ReadOnly -> 4 (* VDISK_READONLY *)
    | ReadWrite -> 0
end

module Media = struct
  type t = CDROM | Disk
  let to_string = function
    | CDROM -> "cdrom"
    | Disk -> "disk"
  let to_int = function
    | CDROM -> 1 (* VDISK_CDROM *)
    | Disk  -> 0
end

module State = struct
  type t = Initialising | InitWait | Initialised | Connected | Closing | Closed
  let table = [
    1, Initialising;
    2, InitWait;
    3, Initialised;
    4, Connected;
    5, Closing;
    6, Closed
  ]
  let table' = List.map (fun (x, y) -> y, x) table
  let to_string t = string_of_int (List.assoc t table' )
  let of_string t = try Some (List.assoc (int_of_string t) table) with _ -> None

  let _state = "state"
  let to_assoc_list t = [
    _state, string_of_int (List.assoc t table')
  ]
end

module Connection = struct
  type t = {
    virtual_device: string;
    backend_path: string;
    backend_domid: int;
    frontend_path: string;
    frontend_domid: int;
    mode: Mode.t;
    media: Media.t;
    removable: bool;
  }

  let to_assoc_list t =
    let backend = [
      "frontend", t.frontend_path;
      "frontend-id", string_of_int t.frontend_domid;
      "online", "1";
      "removable", if t.removable then "1" else "0";
      "state", State.to_string State.Initialising;
      "mode", Mode.to_string t.mode;
    ] in
    let frontend = [
      "backend", t.backend_path;
      "backend-id", string_of_int t.backend_domid;
      "state", State.to_string State.Initialising;
      "virtual-device", t.virtual_device;
      "device-type", Media.to_string t.media;
    ] in
    [
      t.backend_domid, (t.backend_path, "");
      t.frontend_domid, (t.frontend_path, "");
    ]
    @ (List.map (fun (k, v) -> t.backend_domid, (Printf.sprintf "%s/%s" t.backend_path k, v)) backend)
    @ (List.map (fun (k, v) -> t.frontend_domid, (Printf.sprintf "%s/%s" t.frontend_path k, v)) frontend)
end

module Protocol = struct
  type t = X86_64 | X86_32 | Native

  let of_string = function
    | "x86_32-abi" -> Ok X86_32
    | "x86_64-abi" -> Ok X86_64
    | "native"     -> Ok Native
    | x            -> Error (`Msg ("unknown protocol: " ^ x))

  let to_string = function
    | X86_64 -> "x86_64-abi"
    | X86_32 -> "x86_32-abi"
    | Native -> "native"
end

let max_segments_per_request = 256

module FeatureIndirect = struct
  type t = {
    max_indirect_segments: int;
  }

  let _max_indirect_segments = "feature-max-indirect-segments"

  let to_assoc_list t =
    if t.max_indirect_segments = 0
    then [] (* don't advertise the feature *)
    else [ _max_indirect_segments, string_of_int t.max_indirect_segments ]

end

module DiskInfo = struct
  type t = {
    sector_size: int;
    sectors: int64;
    media: Media.t;
    mode: Mode.t;
  }

  let _sector_size = "sector-size"
  let _sectors = "sectors"
  let _info = "info"

  let to_assoc_list t = [
    _sector_size, string_of_int t.sector_size;
    _sectors, Int64.to_string t.sectors;
    _info, string_of_int (Media.to_int t.media lor (Mode.to_int t.mode));
  ]

end

module RingInfo = struct
  type t = {
    ref: int32;
    event_channel: int;
    protocol: Protocol.t;
  }

  let to_string t =
    Printf.sprintf "{ ref = %ld; event_channel = %d; protocol = %s }"
    t.ref t.event_channel (Protocol.to_string t.protocol)

  let _ring_ref = "ring-ref"
  let _event_channel = "event-channel"
  let _protocol = "protocol"

  let keys = [
    _ring_ref;
    _event_channel;
    _protocol;
  ]

  let of_assoc_list l =
    list l _ring_ref >>= fun x -> int32 x
    >>= fun ref ->
    list l _event_channel >>= fun x -> int x
    >>= fun event_channel ->
    list l _protocol >>= fun x -> Protocol.of_string x
    >>= fun protocol ->
    Ok { ref; event_channel; protocol }
end

module Hotplug = struct
  let _hotplug_status = "hotplug-status"
  let _online = "online"
  let _params = "params"
end

(* Block requests; see include/xen/io/blkif.h *)
module Req = struct

  (* Defined in include/xen/io/blkif.h, BLKIF_REQ_* *)
  [%%cenum
  type op =
    | Read          [@id 0]
    | Write         [@id 1]
    | Write_barrier [@id 2]
    | Flush         [@id 3]
    | Op_reserved_1 [@id 4] (* SLES device-specific packet *)
    | Trim          [@id 5]
    | Indirect_op   [@id 6]
    [@@int8_t]
  ]

  let string_of_op = function
  | Read -> "Read" | Write -> "Write" | Write_barrier -> "Write_barrier"
  | Flush -> "Flush" | Op_reserved_1 -> "Op_reserved_1" | Trim -> "Trim"
  | Indirect_op -> "Indirect_op"

  (* Defined in include/xen/io/blkif.h BLKIF_MAX_SEGMENTS_PER_REQUEST *)
  let segments_per_request = 11

  type seg = {
    gref: OS.Xen.Gntref.t;
    first_sector: int;
    last_sector: int;
  }

  type segs =
  | Direct of seg array
  | Indirect of int32 array

  (* Defined in include/xen/io/blkif.h : blkif_request_t *)
  type t = {
    op: op option;
    handle: int;
    id: int64;
    sector: int64;
    nr_segs: int;
    segs: segs;
  }

  (* The segment looks the same in both 32-bit and 64-bit versions *)
  [%%cstruct
  type segment = {
    gref: uint32_t;
    first_sector: uint8_t;
    last_sector: uint8_t;
    _padding: uint16_t;
  } [@@little_endian]
  ]
  let _ = assert (sizeof_segment = 8)

  let get_segments payload nr_segs =
    Array.init nr_segs (fun i ->
      let seg = Cstruct.shift payload (i * sizeof_segment) in {
        gref = OS.Xen.Gntref.of_int32 @@ get_segment_gref seg;
        first_sector = get_segment_first_sector seg;
        last_sector = get_segment_last_sector seg;
      })

  (* The request header has a slightly different format caused by
     not using __attribute__(packed) and letting the C compiler pad *)
  module type DIRECT = sig
    val sizeof_hdr: int
    val get_hdr_op: Cstruct.t -> int
    val set_hdr_op: Cstruct.t -> int -> unit
    val get_hdr_nr_segs: Cstruct.t -> int
    val set_hdr_nr_segs: Cstruct.t -> int -> unit
    val get_hdr_handle: Cstruct.t -> int
    val set_hdr_handle: Cstruct.t -> int -> unit
    val get_hdr_id: Cstruct.t -> int64
    val set_hdr_id: Cstruct.t -> int64 -> unit
    val get_hdr_sector: Cstruct.t -> int64
    val set_hdr_sector: Cstruct.t -> int64 -> unit
  end

  (* The indirect requests have one extra field, and other fields
     have been shuffled *)
  module type INDIRECT = sig
    include DIRECT
    val get_hdr_indirect_op: Cstruct.t -> int
    val set_hdr_indirect_op: Cstruct.t -> int -> unit
  end
  module type PROTOCOL_IMPLEMENTATION = sig
    val total_size : int
    val segments_per_indirect_page : int
    val write_segments : seg array -> Cstruct.t -> unit
    val write_request : t -> Cstruct.t -> int64
    val read_request : Cstruct.t -> t
  end
  module Marshalling(D: DIRECT)(I: INDIRECT) : PROTOCOL_IMPLEMENTATION = struct
    (* total size of a request structure, in bytes *)
    let total_size = D.sizeof_hdr + (sizeof_segment * segments_per_request)

    let page_size = Io_page.round_to_page_size 1
    let segments_per_indirect_page = page_size / sizeof_segment

    let write_segments segs buffer =
      Array.iteri (fun i seg ->
        let buf = Cstruct.shift buffer (i * sizeof_segment) in
        set_segment_gref buf (OS.Xen.Gntref.to_int32 seg.gref);
        set_segment_first_sector buf seg.first_sector;
        set_segment_last_sector buf seg.last_sector
      ) segs

    (* Write a request to a slot in the shared ring. *)
    let write_request req (slot: Cstruct.t) = match req.segs with
      | Direct segs ->
        D.set_hdr_op slot (match req.op with None -> -1 | Some x -> op_to_int x);
        D.set_hdr_nr_segs slot req.nr_segs;
        D.set_hdr_handle slot req.handle;
        D.set_hdr_id slot req.id;
        D.set_hdr_sector slot req.sector;
        let payload = Cstruct.shift slot D.sizeof_hdr in
        write_segments segs payload;
        req.id
      | Indirect refs ->
        I.set_hdr_op slot (op_to_int Indirect_op);
        I.set_hdr_indirect_op slot (match req.op with None -> -1 | Some x -> op_to_int x);
        I.set_hdr_nr_segs slot req.nr_segs;
        I.set_hdr_handle slot req.handle;
        I.set_hdr_id slot req.id;
        I.set_hdr_sector slot req.sector;
        let payload = Cstruct.shift slot I.sizeof_hdr in
        Array.iteri (fun i gref -> Cstruct.LE.set_uint32 payload (i * 4) gref) refs;
        req.id

    let read_request slot =
      let op = int_to_op (D.get_hdr_op slot) in
      if op = Some Indirect_op then begin
        let nr_segs = I.get_hdr_nr_segs slot in
        let nr_grefs = (nr_segs + 511) / 512 in
        let payload = Cstruct.shift slot I.sizeof_hdr in
        let grefs = Array.init nr_grefs (fun i -> Cstruct.LE.get_uint32 payload (i * 4)) in {
          op = int_to_op (I.get_hdr_indirect_op slot); (* the "real" request type *)
          handle = I.get_hdr_handle slot; id = I.get_hdr_id slot;
          sector = I.get_hdr_sector slot; nr_segs;
          segs = Indirect grefs
        }
      end else begin
        let payload = Cstruct.shift slot D.sizeof_hdr in
        let segs = get_segments payload (D.get_hdr_nr_segs slot) in {
          op; handle = D.get_hdr_handle slot; id = D.get_hdr_id slot;
          sector = D.get_hdr_sector slot; nr_segs = D.get_hdr_nr_segs slot;
          segs = Direct segs
        }
      end
  end
  module Proto_64 = Marshalling(struct
    [%%cstruct
    type hdr = {
      op: uint8_t;
      nr_segs: uint8_t;
      handle: uint16_t;
      _padding: uint32_t; (* emitted by C compiler *)
      id: uint64_t;
      sector: uint64_t;
    } [@@little_endian]
    ]
  end) (struct
    [%%cstruct
    type hdr = {
      op: uint8_t;
      indirect_op: uint8_t;
      nr_segs: uint16_t;
      _padding1: uint32_t;
      id: uint64_t;
      sector: uint64_t;
      handle: uint16_t;
      _padding2: uint16_t;
      (* up to 8 grant references *)
    } [@@little_endian]
    ]
  end)

  module Proto_32 = Marshalling(struct
    [%%cstruct
    type hdr = {
      op: uint8_t;
      nr_segs: uint8_t;
      handle: uint16_t;
      (* uint32_t       _padding; -- not included *)
      id: uint64_t;
      sector: uint64_t;
    } [@@little_endian]
    ]
  end) (struct
    [%%cstruct
    type hdr = {
      op: uint8_t;
      indirect_op: uint8_t;
      nr_segs: uint16_t;
      id: uint64_t;
      sector: uint64_t;
      handle: uint16_t;
      _padding1: uint16_t;
      (* up to 8 grant references *)
    } [@@little_endian]
    ]
  end)
end

module Res = struct

  (* Defined in include/xen/io/blkif.h, BLKIF_RSP_* *)
  [%%cenum
  type rsp =
    | OK            [@id 0]
    | Error         [@id 0xffff]
    | Not_supported [@id 0xfffe]
    [@@uint16_t]
  ]
  (* Defined in include/xen/io/blkif.h, blkif_response_t *)
  type t = {
    op: Req.op option;
    st: rsp option;
  }

  (* The same structure is used in both the 32- and 64-bit protocol versions,
     modulo the extra padding at the end. *)
  [%%cstruct
  type response_hdr = {
    id: uint64_t;
    op: uint8_t;
    _padding: uint8_t;
    st: uint16_t;
    (* 64-bit only but we don't need to care since there aren't any more fields: *)
    _padding2: uint32_t;
  } [@@little_endian]
  ]
  let write_response (id, t) slot =
    set_response_hdr_id slot id;
    set_response_hdr_op slot (match t.op with None -> -1 | Some x -> Req.op_to_int x);
    set_response_hdr_st slot (match t.st with None -> -1 | Some x -> rsp_to_int x)

  let read_response slot =
    get_response_hdr_id slot, {
      op = Req.int_to_op (get_response_hdr_op slot);
      st = int_to_rsp (get_response_hdr_st slot)
    }
end
