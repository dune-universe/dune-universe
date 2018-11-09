module KOI8_U = KOI8_U
module KOI8_R = KOI8_R

let io_buffer_size = 65536

type encoding = [`KOI8_U | `KOI8_R]

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len =
  invalid_arg "Invalid bounds (off: %d, len: %d)" off len

let strf = Format.asprintf

let pp = Format.fprintf

let encoding_of_string = function
  | "KOI8-R" | "csKOI8R" -> `KOI8_R
  | "KOI8-U" | "csKOI8U" -> `KOI8_U
  | s -> invalid_arg "Invalid character-sets: %s" s

let encoding_to_string = function `KOI8_R -> "KOI8-R" | `KOI8_U -> "KOI8-U"

let table = function `KOI8_R -> KOI8_R.map | `KOI8_U -> KOI8_U.map

let malformed kind byte =
  `Malformed
    (strf "Byte %02x is not a valid %s codepoint" byte
       (encoding_to_string kind))

let uchar ucp = `Uchar (Uchar.of_int ucp)

let unsafe_byte source off pos =
  Char.code (Bytes.unsafe_get source (off + pos))

let unsafe_bytes_iteri f s =
  let l = Bytes.length s in
  for i = 0 to l - 1 do
    f i (unsafe_byte s 0 i)
  done

let r kind source off pos =
  (* XXX(dinosaure): assert (0 <= off && 0 < pos && off + pos < Bytes.length source) *)
  try
    let byte = unsafe_byte source off pos in
    let code = (table kind).(byte) in
    if code = -1 then malformed kind byte else uchar code
  with Invalid_argument _ ->
    let byte = unsafe_byte source off pos in
    malformed kind byte

(* Decode *)

type src = [`Channel of in_channel | `String of string | `Manual]

type decode = [`Await | `End | `Uchar of Uchar.t | `Malformed of string]

let pp_decode ppf = function
  | `Uchar u -> pp ppf "@[`Uchar U+%04X@]" (Uchar.to_int u)
  | `End -> pp ppf "`End"
  | `Await -> pp ppf "`Await"
  | `Malformed e -> pp ppf "@[`Malformed (%s)@]" e

type 'kind decoder =
  { src: src
  ; kind: 'kind
  ; mutable i_off: int
  ; mutable i_pos: int
  ; mutable i_len: int
  ; mutable i: Bytes.t
  ; mutable byte_count: int
  ; mutable pp:
      'kind decoder -> [`Malformed of string | `Uchar of Uchar.t] -> decode
  ; mutable k: 'kind decoder -> decode }
  constraint 'kind = [< encoding]

let end_of_input decoder =
  decoder.i <- Bytes.empty ;
  decoder.i_off <- 0 ;
  decoder.i_pos <- 0 ;
  decoder.i_len <- min_int

(* XXX(dinosaure): return [`End] only when [rem < 0]. *)

let src decoder source off len =
  if off < 0 || len < 0 || off + len > Bytes.length source then
    invalid_bounds off len
  else if len = 0 then end_of_input decoder
  else (
    decoder.i <- source ;
    decoder.i_off <- off ;
    decoder.i_pos <- 0 ;
    decoder.i_len <- len - 1 )

let refill k decoder =
  match decoder.src with
  | `Manual ->
      decoder.k <- k ;
      `Await
  | `String _ -> end_of_input decoder ; k decoder
  | `Channel ic ->
      let len = input ic decoder.i 0 (Bytes.length decoder.i) in
      src decoder decoder.i 0 len ;
      k decoder

let ret k value succ decoder =
  decoder.k <- k ;
  decoder.byte_count <- decoder.byte_count + succ ;
  decoder.pp decoder value

let i_rem decoder = decoder.i_len - decoder.i_pos + 1

let rec decode_koi8 decoder =
  let rem = i_rem decoder in
  if rem <= 0 then if rem < 0 then `End else refill decode_koi8 decoder
  else
    let off = decoder.i_off in
    let pos = decoder.i_pos in
    decoder.i_pos <- decoder.i_pos + 1 ;
    ret decode_koi8 (r decoder.kind decoder.i off pos) 1 decoder

let pp_koi8 _decoder v = (v :> decode)

let decoder kind src =
  let pp = pp_koi8 in
  let k = decode_koi8 in
  let i, i_off, i_pos, i_len =
    match src with
    | `Manual -> (Bytes.empty, 0, 1, 0)
    | `Channel _ -> (Bytes.create io_buffer_size, 0, 1, 0)
    | `String s -> (Bytes.unsafe_of_string s, 0, 0, String.length s - 1)
  in
  {src; kind; i_off; i_pos; i_len; i; byte_count= 0; pp; k}

let decode decoder = decoder.k decoder

let decoder_byte_count decoder = decoder.byte_count

let decoder_src decoder = decoder.src

let decoder_kind decoder = decoder.kind

module Char = struct
  let is_valid kind byte =
    let code = Char.code byte in
    if (table kind).(code) = -1 then false else true

  let equal _kind = Char.equal

  let compare _kind = Char.compare

  let unicode kind byte =
    let code = Char.code byte in
    let unicode = (table kind).(code) in
    if unicode = -1 then
      invalid_arg "Byte %02x is not a valid %s codepoint" code
        (encoding_to_string kind) ;
    Uchar.of_int unicode
end

module String = struct
  type 'a folder =
    'a -> int -> [`Malformed of string | `Uchar of Uchar.t] -> 'a

  let fold kind ?off ?len folder acc str =
    let off, len =
      match (off, len) with
      | Some off, Some len -> (off, len)
      | None, Some len -> (0, len)
      | Some off, None -> (off, String.length str - off)
      | None, None -> (0, String.length str)
    in
    let acc = ref acc in
    let go idx byte =
      let unicode = (table kind).(byte) in
      let res = if unicode = -1 then malformed kind byte else uchar unicode in
      acc := folder !acc idx res
    in
    unsafe_bytes_iteri go (Bytes.unsafe_of_string (String.sub str off len)) ;
    !acc
end
