(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-flac.
 *
 * Ocaml-flac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-flac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-flac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Author; Romain Beauxis <toots@rastageeks.org> *)

exception Internal

let () = Callback.register_exception "flac_exn_internal" Internal

module Decoder = struct
  type 'a dec
  type 'a t = 'a dec
  type write = float array array -> unit
  type read = bytes -> int -> int -> int

  type 'a callbacks = {
    read : read;
    seek : (int64 -> unit) option;
    tell : (unit -> int64) option;
    length : (unit -> int64) option;
    eof : (unit -> bool) option;
    write : write;
  }

  type generic

  let get_callbacks ?seek ?tell ?length ?eof read write : generic callbacks =
    { read; seek; tell; length; eof; write }

  (** Possible states of a decoder. *)
  type state =
    [ `Search_for_metadata
    | `Read_metadata
    | `Search_for_frame_sync
    | `Read_frame
    | `End_of_stream
    | `Ogg_error
    | `Seek_error
    | `Aborted
    | `Memory_allocation_error
    | `Uninitialized ]

  exception Lost_sync
  exception Bad_header
  exception Frame_crc_mismatch
  exception Unparseable_stream
  exception Not_flac

  let () =
    Callback.register_exception "flac_dec_exn_lost_sync" Lost_sync;
    Callback.register_exception "flac_dec_exn_bad_header" Bad_header;
    Callback.register_exception "flac_dec_exn_crc_mismatch" Frame_crc_mismatch;
    Callback.register_exception "flac_dec_exn_unparseable_stream"
      Unparseable_stream

  type info = {
    sample_rate : int;
    channels : int;
    bits_per_sample : int;
    total_samples : int64;
    md5sum : string;
  }

  type comments = string * (string * string) list
  type comments_array = string * string array

  external info : 'a dec -> info * comments_array option
    = "ocaml_flac_decoder_info"

  let split_comment comment =
    try
      let equal_pos = String.index_from comment 0 '=' in
      let c1 = String.uppercase_ascii (String.sub comment 0 equal_pos) in
      let c2 =
        String.sub comment (equal_pos + 1)
          (String.length comment - equal_pos - 1)
      in
      (c1, c2)
    with Not_found -> (comment, "")

  let _comments cmts =
    match cmts with
      | None -> None
      | Some (vd, cmts) ->
          Some (vd, Array.to_list (Array.map split_comment cmts))

  let info x =
    try
      let info, comments = info x in
      (info, _comments comments)
    with Internal -> raise Not_flac

  external state : 'a t -> 'a callbacks -> state = "ocaml_flac_decoder_state"
  external create : 'a callbacks -> 'a dec = "ocaml_flac_decoder_create"
  external init : 'a dec -> 'a callbacks -> unit = "ocaml_flac_decoder_init"

  let init dec c =
    init dec c;
    let info, comments = info dec in
    (dec, info, comments)

  external process : 'a t -> 'a callbacks -> unit = "ocaml_flac_decoder_process"

  external seek : 'a t -> 'a callbacks -> Int64.t -> bool
    = "ocaml_flac_decoder_seek"

  external flush : 'a t -> 'a callbacks -> bool = "ocaml_flac_decoder_flush"
  external reset : 'a t -> 'a callbacks -> bool = "ocaml_flac_decoder_reset"
  external to_s16le : float array array -> string = "caml_flac_float_to_s16le"

  module File = struct
    type file

    type handle = {
      fd : Unix.file_descr;
      dec : file t;
      callbacks : file callbacks;
      info : info;
      comments : (string * (string * string) list) option;
    }

    let create_from_fd write fd =
      let read = Unix.read fd in
      let seek n =
        let n = Int64.to_int n in
        ignore (Unix.lseek fd n Unix.SEEK_SET)
      in
      let tell () = Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR) in
      let length () =
        let stats = Unix.fstat fd in
        Int64.of_int stats.Unix.st_size
      in
      let eof () =
        let stats = Unix.fstat fd in
        Unix.lseek fd 0 Unix.SEEK_CUR = stats.Unix.st_size
      in
      let callbacks =
        {
          read;
          seek = Some seek;
          tell = Some tell;
          length = Some length;
          eof = Some eof;
          write;
        }
      in
      let dec = create callbacks in
      let dec, info, comments = init dec callbacks in
      { fd; comments; callbacks; dec; info }

    let create write filename =
      let fd = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
      try create_from_fd write fd
      with e ->
        Unix.close fd;
        raise e
  end
end

module Encoder = struct
  type 'a priv
  type write = bytes -> unit

  type 'a callbacks = {
    write : write;
    seek : (int64 -> unit) option;
    tell : (unit -> int64) option;
  }

  type generic

  let get_callbacks ?seek ?tell write : generic callbacks =
    { write; seek; tell }

  type params = {
    channels : int;
    bits_per_sample : int;
    sample_rate : int;
    compression_level : int option;
    total_samples : int64 option;
  }

  type comments = (string * string) list
  type 'a t = 'a priv * params

  exception Invalid_data

  external create : (string * string) array -> params -> 'a callbacks -> 'a priv
    = "ocaml_flac_encoder_create"

  let create ?(comments = []) p c =
    if p.channels <= 0 then raise Invalid_data;
    let comments = Array.of_list comments in
    let enc = create comments p c in
    (enc, p)

  external process : 'a priv -> 'a callbacks -> float array array -> int -> unit
    = "ocaml_flac_encoder_process"

  let process (enc, p) c data =
    if Array.length data <> p.channels then raise Invalid_data;
    process enc c data p.bits_per_sample

  external finish : 'a priv -> 'a callbacks -> unit
    = "ocaml_flac_encoder_finish"

  let finish (enc, _) c = finish enc c

  external from_s16le : string -> int -> float array array
    = "caml_flac_s16le_to_float"

  module File = struct
    type file

    type handle = {
      fd : Unix.file_descr;
      enc : file t;
      callbacks : file callbacks;
    }

    let create_from_fd ?comments params fd =
      let write s =
        let len = Bytes.length s in
        let rec f pos =
          if pos < len then (
            let ret = Unix.write fd s pos (len - pos) in
            f (pos + ret) )
        in
        f 0
      in
      let seek n =
        let n = Int64.to_int n in
        ignore (Unix.lseek fd n Unix.SEEK_SET)
      in
      let tell () = Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR) in
      let callbacks = { write; seek = Some seek; tell = Some tell } in
      let enc = create ?comments params callbacks in
      { fd; enc; callbacks }

    let create ?comments params filename =
      let fd = Unix.openfile filename [Unix.O_CREAT; Unix.O_RDWR] 0o640 in
      create_from_fd ?comments params fd
  end
end
