(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* NBD client library *)

open Sexplib.Std
open Result

(* We need to serialise/deserialise result values *)
type ('a, 'b) _result = [ `Ok of 'a | `Error of 'b ] [@@deriving sexp]
let result_of_sexp a b s = match _result_of_sexp a b s with
  | `Ok x -> Ok x | `Error y -> Error y
let sexp_of_result a b r =
  sexp_of__result a b (match r with Ok x -> `Ok x | Error y -> `Error y)

let nbd_cmd_read = 0l
let nbd_cmd_write = 1l
let nbd_cmd_disc = 2l
let nbd_cmd_flush = 3l
let nbd_cmd_trim = 4l

let nbd_request_magic = 0x25609513l
let nbd_reply_magic = 0x67446698l

let nbd_flag_has_flags = 1
let nbd_flag_read_only = 2
let nbd_flag_send_flush = 4
let nbd_flag_send_fua = 8
let nbd_flag_rotational = 16
let nbd_flag_send_trim = 32

let nbd_flag_fixed_newstyle = 1
let nbd_flag_no_zeroes = 2

let nbd_flag_c_fixed_newstyle = 1
let nbd_flag_c_no_zeroes = 2

let zero buf =
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done

module PerExportFlag = struct
  type t =
    | Read_only
    | Send_flush
    | Send_fua
    | Rotational
    | Send_trim
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 x =
    let flags = Int32.to_int x in
    let is_set i mask = i land mask = mask in
    List.map snd
      (List.filter (fun (mask,_) -> is_set flags mask)
         [ nbd_flag_read_only, Read_only;
           nbd_flag_send_flush, Send_flush;
           nbd_flag_send_fua, Send_fua;
           nbd_flag_rotational, Rotational;
           nbd_flag_send_trim, Send_trim; ])

  let to_int flags =
    let one = function
      | Read_only -> nbd_flag_read_only
      | Send_flush -> nbd_flag_send_flush
      | Send_fua -> nbd_flag_send_fua
      | Rotational -> nbd_flag_rotational
      | Send_trim -> nbd_flag_send_trim in
    List.fold_left (lor) nbd_flag_has_flags (List.map one flags)

  let to_int32 flags = Int32.of_int (to_int flags)
end

module GlobalFlag = struct
  type t =
    | Fixed_newstyle
    | No_zeroes
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int flags =
    let is_set i mask = i land mask = mask in
    List.map snd
      (List.filter (fun (mask,_) -> is_set flags mask)
         [ nbd_flag_fixed_newstyle, Fixed_newstyle;
           nbd_flag_no_zeroes, No_zeroes; ])

  let to_int flags =
    let one = function
      | Fixed_newstyle -> nbd_flag_fixed_newstyle
      | No_zeroes -> nbd_flag_no_zeroes in
    List.fold_left (lor) 0 (List.map one flags)

end

module ClientFlag = struct
  type t =
    | Fixed_newstyle
    | No_zeroes
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 flags =
    let flags = Int32.to_int flags in
    let is_set i mask = i land mask = mask in
    List.map snd
      (List.filter (fun (mask,_) -> is_set flags mask)
         [ nbd_flag_c_fixed_newstyle, Fixed_newstyle;
           nbd_flag_c_no_zeroes, No_zeroes; ])

  let to_int32 flags =
    let one = function
      | Fixed_newstyle -> nbd_flag_c_fixed_newstyle
      | No_zeroes -> nbd_flag_c_no_zeroes in
    Int32.of_int (List.fold_left (lor) 0 (List.map one flags))

end

module Error = struct
  type t = [
    | `EPERM
    | `EIO
    | `ENOMEM
    | `EINVAL
    | `ENOSPC
    | `Unknown of int32
  ] [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 = function
    | 1l -> `EPERM
    | 5l -> `EIO
    | 12l -> `ENOMEM
    | 22l -> `EINVAL
    | 28l -> `ENOSPC
    | x -> `Unknown x

  let to_int32 = function
    | `EPERM -> 1l
    | `EIO -> 5l
    | `ENOMEM -> 12l
    | `EINVAL -> 22l
    | `ENOSPC -> 28l
    | `Unknown x -> x
end

module Command = struct
  type t =
    | Read
    | Write
    | Disc
    | Flush
    | Trim
    | Unknown of int32
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 = function
    | 0l -> Read
    | 1l -> Write
    | 2l -> Disc
    | 3l -> Flush
    | 4l -> Trim
    | c  -> Unknown c

  let to_int32 = function
    | Read -> 0l
    | Write -> 1l
    | Disc -> 2l
    | Flush -> 3l
    | Trim -> 4l
    | Unknown c -> c

end

module Option = struct
  type t =
    | ExportName
    | Abort
    | List
    | Unknown of int32
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 = function
    | 1l -> ExportName
    | 2l -> Abort
    | 3l -> List
    | c -> Unknown c

  let to_int32 = function
    | ExportName -> 1l
    | Abort -> 2l
    | List -> 3l
    | Unknown c -> c
end

module OptionResponse = struct
  type t =
    | Ack
    | Server
    | Unsupported
    | Policy
    | Invalid
    | Platform
    | Unknown of int32
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 = function
    | 1l -> Ack
    | 2l -> Server
    | -2147483647l -> Unsupported
    | -2147483646l -> Policy
    | -2147483645l -> Invalid
    | -2147483644l -> Platform
    | x -> Unknown x

  let to_int32 = function
    | Ack -> 1l
    | Server -> 2l
    | Unsupported -> -2147483647l
    | Policy -> -2147483646l
    | Invalid -> -2147483645l
    | Platform -> -2147483644l;
    | Unknown x -> x

end

(* Sent by the server to the client which includes an initial
   protocol choice *)
module Announcement = struct
  type t = [ `V1 | `V2 ] [@@deriving sexp]

  [%%cstruct
  type t = {
      passwd: uint8_t [@len 8];
      magic:  uint64_t;
    } [@@big_endian]
  ]

  let sizeof = sizeof_t

  let expected_passwd = "NBDMAGIC"

  let v1_magic = 0x00420281861253L
  let v2_magic = 0x49484156454F5054L

  let marshal buf t =
    set_t_passwd expected_passwd 0 buf;
    set_t_magic buf (match t with `V1 -> v1_magic | `V2 -> v2_magic)

  let unmarshal buf =
    let passwd = Cstruct.to_string (get_t_passwd buf) in
    if passwd <> expected_passwd
    then Error (Failure "Bad magic in negotiate")
    else
      let magic = get_t_magic buf in
      if magic = v1_magic
      then Ok `V1
      else
      if magic = v2_magic
      then Ok `V2
      else Error (Failure (Printf.sprintf "Bad magic; expected %Ld or %Ld got %Ld" v1_magic v2_magic magic))
end

module Negotiate = struct
  type v1 = {
    size: int64;
    flags: PerExportFlag.t list;
  } [@@deriving sexp]

  type v2 = GlobalFlag.t list [@@deriving sexp]

  type t =
    | V1 of v1
    | V2 of v2
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  [%%cstruct
  type v1 = {
      size:    uint64_t;
      flags:   uint32_t;
      padding: uint8_t [@len 124];
    } [@@big_endian]
  ]

  [%%cstruct
  type v2 = {
      flags: uint16_t;
    } [@@big_endian]
  ]
  let sizeof = function
    | `V1 -> sizeof_v1
    | `V2 -> sizeof_v2

  let marshal buf t =
    zero buf;
    match t with
    | V1 t ->
      set_v1_size buf t.size;
      set_v1_flags buf (PerExportFlag.to_int32 t.flags);
    | V2 t ->
      set_v2_flags buf (GlobalFlag.to_int t)

  let unmarshal buf t =
    match t with
    | `V1 ->
      let size = get_v1_size buf in
      let flags = PerExportFlag.of_int32 (get_v1_flags buf) in
      Ok (V1 { size; flags })
    | `V2 ->
      let flags = GlobalFlag.of_int (get_v2_flags buf) in
      Ok (V2 flags)
end

module NegotiateResponse = struct
  type t = ClientFlag.t list [@@deriving sexp]

  let sizeof = 4

  let marshal buf t =
    Cstruct.LE.set_uint32 buf 0 (ClientFlag.to_int32 t)

  let unmarshal buf =
    ClientFlag.of_int32 (Cstruct.LE.get_uint32 buf 0)

end

(* In the 'new' and 'new fixed' protocols, options are preceeded by
   a common header which includes a type and a length. *)
module OptionRequestHeader = struct
  type t = {
    ty: Option.t;
    length: int32;
  } [@@deriving sexp]

  [%%cstruct
  type t = {
      magic:  uint64_t;
      ty:     uint32_t;
      length: uint32_t;
    } [@@big_endian]
  ]
  let sizeof = sizeof_t

  let marshal buf t =
    set_t_magic buf Announcement.v2_magic;
    set_t_ty buf (Option.to_int32 t.ty);
    set_t_length buf t.length

  let unmarshal buf =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if Announcement.v2_magic <> magic
      then Error (Failure (Printf.sprintf "Bad reply magic: expected %Ld, got %Ld" Announcement.v2_magic magic))
      else Ok () ) >>= fun () ->
    let ty = Option.of_int32 (get_t_ty buf) in
    let length = get_t_length buf in
    Ok { ty; length }
end

(* This is the option sent by the client to select a particular disk
   export. *)
module ExportName = struct
  type t = string [@@deriving sexp]

  let sizeof = String.length

  let marshal buf x =
    Cstruct.blit_from_string x 0 buf 0 (String.length x)
end

(* In both the 'new' style handshake and the 'fixed new' style handshake,
   the server will reply to an ExportName option with either a connection
   close or a DiskInfo: *)
module DiskInfo = struct
  type t = {
    size: int64;
    flags: PerExportFlag.t list
  } [@@deriving sexp]

  [%%cstruct
  type t = {
      size:    uint64_t;
      flags:   uint16_t;
      padding: uint8_t  [@len 124];
    } [@@big_endian]
  ]
  let sizeof = sizeof_t

  let unmarshal buf =
    let size = get_t_size buf in
    let flags = PerExportFlag.of_int32 (Int32.of_int (get_t_flags buf)) in
    Ok { size; flags }

  let marshal buf t =
    set_t_size buf t.size;
    set_t_flags buf (PerExportFlag.to_int t.flags)
end

(* In the 'fixed new' style handshake, all options apart from ExportName
   should result in reply packets as follows: *)
module OptionResponseHeader = struct
  [%%cstruct
  type t = {
      magic:         uint64_t;
      request_type:  uint32_t;
      response_type: uint32_t;
      length:        uint32_t;
    } [@@big_endian]
  ]
  type t = {
    request_type: Option.t;
    response_type: OptionResponse.t;
    length: int32;
  } [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let sizeof = sizeof_t

  let expected_magic = 0x3e889045565a9L

  let unmarshal buf =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if expected_magic <> magic
      then Error (Failure (Printf.sprintf "Bad reply magic: expected %Ld, got %Ld" expected_magic magic))
      else Ok () ) >>= fun () ->
    let request_type = Option.of_int32 (get_t_request_type buf) in
    let response_type = OptionResponse.of_int32 (get_t_response_type buf) in
    let length = get_t_length buf in
    Ok { request_type; response_type; length }

  let marshal buf t =
    set_t_magic buf expected_magic;
    set_t_request_type buf (Option.to_int32 t.request_type);
    set_t_response_type buf (OptionResponse.to_int32 t.response_type);
    set_t_length buf t.length
end

(* A description of an export, sent in response to a List option *)
module Server = struct
  type t = {
    name: string;
  } [@@deriving sexp]

  [%%cstruct
  type t = {
      length: uint32_t;
    } [@@big_endian]
  ]
  let sizeof t = sizeof_t + (String.length t.name)

  let unmarshal buf =
    let length = Int32.to_int (get_t_length buf) in
    let buf = Cstruct.shift buf sizeof_t in
    let name = Cstruct.(to_string (sub buf 0 length)) in
    Ok { name }
end

module Request = struct
  type t = {
    ty : Command.t;
    handle : int64;
    from : int64;
    len : int32
  } [@@deriving sexp]

  let to_string t =
    Printf.sprintf "{ Command = %s; handle = %Ld; from = %Ld; len = %ld }"
      (Command.to_string t.ty) t.handle t.from t.len

  [%%cstruct
  type t = {
      magic:  uint32_t;
      ty:     uint32_t;
      handle: uint64_t;
      from:   uint64_t;
      len:    uint32_t;
    } [@@big_endian]
  ]
  let unmarshal (buf: Cstruct.t) =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if nbd_request_magic <> magic
      then Error (Failure (Printf.sprintf "Bad request magic: expected %ld, got %ld" magic nbd_request_magic))
      else Ok () ) >>= fun () ->
    let ty = Command.of_int32 (get_t_ty buf) in
    let handle = get_t_handle buf in
    let from = get_t_from buf in
    let len = get_t_len buf in
    Ok { ty; handle; from; len }

  let sizeof = sizeof_t

  let marshal (buf: Cstruct.t) t =
    set_t_magic buf nbd_request_magic;
    set_t_ty buf (Command.to_int32 t.ty);
    set_t_handle buf t.handle;
    set_t_from buf t.from;
    set_t_len buf t.len
end

module Reply = struct
  type t = {
    error : (unit, Error.t) result;
    handle : int64;
  } [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  [%%cstruct
  type t = {
      magic:  uint32_t;
      error:  uint32_t;
      handle: uint64_t;
    } [@@big_endian]
  ]
  let unmarshal (buf: Cstruct.t) =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if nbd_reply_magic <> magic
      then Error (Failure (Printf.sprintf "Bad reply magic: expected %ld, got %ld" magic nbd_reply_magic))
      else Ok () ) >>= fun () ->
    let error = get_t_error buf in
    let error = if error = 0l then Ok () else Error (Error.of_int32 error) in
    let handle = get_t_handle buf in
    Ok { error; handle }

  let sizeof = sizeof_t

  let marshal (buf: Cstruct.t) t =
    set_t_magic buf nbd_reply_magic;
    let error = match t.error with
      | Ok () -> 0l
      | Error e -> Error.to_int32 e in
    set_t_error buf error;
    set_t_handle buf t.handle
end
