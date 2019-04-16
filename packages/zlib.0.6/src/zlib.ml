(*
 * Copyright (c) 2015, Christopher Zimmermann
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


type status = 
  | Ok                   (* 0 *)
  | Stream_end           (* 1 *)
  | Need_dict            (* 2 *)
  | Buf_error            (* 3 (zlib -5) *)
  | Data_error of string (* 0 (zlib -3) *)

type algo = Deflated

type strategy =
  | Default_strategy  (* 0 *)
  | Filtered          (* 1 *)
  | Huffman_only      (* 2 *)
  | RLE               (* 3 *)
  | Fixed             (* 4 *)

type flush =
  | No_flush          (* 0 *)
  | Partial_flush     (* 1 *)
  | Sync_flush        (* 2 *)
  | Full_flush        (* 3 *)
  | Finish            (* 4 *)
  | Block             (* 5 *)
  | Trees             (* 6 *)

type data_type =
  | Binary            (* 0 *)
  | Text              (* 1 *)
  | Unknown           (* 2 *)

type deflate
type inflate

type 'a state
type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type 'a t =
  { state :'a state
  ; mutable in_buf    :((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t)
  ; mutable out_buf   :((char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t)
  ; mutable in_ofs    :int
  ; mutable out_ofs   :int
  ; mutable in_len    :int
  ; mutable out_len   :int
  ; mutable in_total  :int
  ; mutable out_total :int
  ; mutable data_type :int
  ; mutable cksum     :int32
  }

type header =
  { text        :bool
  ; mtime       :int32
  ; os          :int
  ; xflags      :int
  ; extra       :string option
  ; name        :string option
  ; comment     :string option
  }

external inflate_init : window_bits:int -> inflate state = "zlib_inflate_init"
external deflate_init :
  level:int -> algo:algo -> window_bits:int -> memory:int -> strategy:strategy
  -> deflate state
  = "zlib_deflate_init"

(* calculate upper bound on deflated stream. *)
external deflate_bound : deflate state -> int -> int
  = "zlib_deflate_bound"

(* flate handle flush *)
external flate : 'a t -> flush -> status = "zlib_flate"

(* set dictionary *)
external deflate_set_dictionary : deflate state -> string -> int32
  = "zlib_deflate_set_dictionary"
external inflate_set_dictionary : inflate state -> string -> status
  = "zlib_inflate_set_dictionary"

(* set/get header *)
external set_header : deflate state -> header -> unit = "zlib_set_header"
external get_header : inflate state -> header         = "zlib_get_header"

(* reset *)
external reset : 'a t -> unit = "zlib_reset"

(* adler32 *)
external adler32 : int32 -> string -> int32 = "zlib_adler32"
let adler32_empty = Int32.one

let get_data_type (mlstate :deflate t) =
  match mlstate.data_type with
  |0 -> Binary
  |1 -> Text
  |2 -> Unknown
  |_ -> assert(false)
;;

(* create caml record wrapping zlib state and bigarray buffers *)0
let create_deflate, create_inflate =
  let dummy_buf = Bigarray.(Array1.create char c_layout 0) in
  let wrap state =
    { state
    ; in_buf    = dummy_buf
    ; out_buf   = dummy_buf
    ; in_ofs    = 0
    ; out_ofs   = 0
    ; in_len    = -1
    ; out_len   = -1
    ; in_total  = 0
    ; out_total = 0
    ; cksum     = Int32.zero
    ; data_type = 2
    }
  in
  let create_deflate
      ?(level=(-1))
      ?(algo=Deflated)
      ?(window_bits=15)
      ?(memory=8)
      ?(strategy=Default_strategy)
      () =
    wrap (deflate_init ~level ~algo ~window_bits ~memory ~strategy)
  in
  let create_inflate ?(window_bits=15) () =
    wrap (inflate_init ~window_bits)
  in
  create_deflate, create_inflate
