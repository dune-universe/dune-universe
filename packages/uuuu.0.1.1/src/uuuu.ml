module ISO_8859_1 = ISO_8859_1
module ISO_8859_2 = ISO_8859_2
module ISO_8859_3 = ISO_8859_3
module ISO_8859_4 = ISO_8859_4
module ISO_8859_5 = ISO_8859_5
module ISO_8859_6 = ISO_8859_6
module ISO_8859_7 = ISO_8859_7
module ISO_8859_8 = ISO_8859_8
module ISO_8859_9 = ISO_8859_9
module ISO_8859_10 = ISO_8859_10
module ISO_8859_11 = ISO_8859_11
module ISO_8859_13 = ISO_8859_13
module ISO_8859_14 = ISO_8859_14
module ISO_8859_15 = ISO_8859_15
module ISO_8859_16 = ISO_8859_16

(* XXX(dinosaure): IO_BUFFER_SIZE *)
let io_buffer_size = 65536

(* XXX(dinosaure): ISO 8859 encoding schemes. *)
type encoding =
  [ `ISO_8859_1
  | `ISO_8859_2
  | `ISO_8859_3
  | `ISO_8859_4
  | `ISO_8859_5
  | `ISO_8859_6
  | `ISO_8859_7
  | `ISO_8859_8
  | `ISO_8859_9
  | `ISO_8859_10
  | `ISO_8859_11
  | `ISO_8859_13
  | `ISO_8859_14
  | `ISO_8859_15
  | `ISO_8859_16 ]

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let invalid_bounds off len =
  invalid_arg "Invalid bounds (off: %d, len: %d)" off len

let strf = Format.asprintf

let pp = Format.fprintf

(* XXX(dinosaure): see IANA character-sets database. *)

let encoding_of_string = function
  | "ISO_8859-1:1987" | "iso-ir-100" | "ISO_8859-1" | "ISO-8859-1" | "latin1"
   |"l1" | "IBM819" | "CP819" | "csISOLatin1" ->
      `ISO_8859_1
  | "ISO_8859-2:1987" | "iso-ir-101" | "ISO_8859-2" | "ISO-8859-2" | "latin2"
   |"l2" | "csISOLatin2" ->
      `ISO_8859_2
  | "ISO_8859-3:1988" | "iso-ir-109" | "ISO_8859-3" | "ISO-8859-3" | "latin3"
   |"l3" | "csISOLatin3" ->
      `ISO_8859_3
  | "ISO_8859-4:1988" | "iso-ir-110" | "ISO_8859-4" | "ISO-8859-4" | "latin4"
   |"l4" | "csISOLatin4" ->
      `ISO_8859_4
  | "ISO_8859-5:1988" | "iso-ir-144" | "ISO_8859-5" | "ISO-8859-5"
   |"cyrillic" | "csISOLatinCyrillic" ->
      `ISO_8859_5
  | "ISO_8859-6:1987" | "iso-ir-127" | "ISO_8859-6" | "ISO-8859-6" | "arabic"
   |"ECMA-114" | "ASMO-708" | "csISOLatinArabic" ->
      `ISO_8859_6
  | "ISO_8859-7:1987" | "iso-ir-126" | "ISO_8859-7" | "ISO-8859-7" | "greek"
   |"greek8" | "ELOT_928" | "ECMA-118" | "csISOLatinGreek" ->
      `ISO_8859_7
  | "ISO_8859-8:1988" | "iso-ir-138" | "ISO_8859-8" | "ISO-8859-8" | "hebrew"
   |"csISOLatinHebrew" ->
      `ISO_8859_8
  | "ISO_8859-9:1989" | "iso-ir-148" | "ISO_8859-9" | "ISO-8859-9" | "latin5"
   |"l5" | "csISOLatin5" ->
      `ISO_8859_9
  | "ISO_8859-10:1992" | "iso-ir-157" | "ISO-8859-10" | "latin6" | "l6"
   |"csISOLatin6" ->
      `ISO_8859_10
  | "ISO-8859-11" | "TIS-620" | "csTIS620" -> `ISO_8859_11
  | "ISO-8859-13" | "csISO885913" -> `ISO_8859_13
  | "ISO_8859-14:1998" | "iso-ir-199" | "ISO_8859-14" | "ISO-8859-14"
   |"latin8" | "l8" | "iso-celtic" | "csISO885914" ->
      `ISO_8859_14
  | "ISO_8859-15" | "ISO-8859-15" | "Latin-9" | "csISO885915" -> `ISO_8859_15
  | "ISO_8859-16:2001" | "iso-ir-226" | "ISO_8859-16" | "ISO-8859-16"
   |"latin10" | "l10" | "csISO885916" ->
      `ISO_8859_16
  | s -> invalid_arg "Invalid character-sets: %s" s

let encoding_to_string = function
  | `ISO_8859_1 -> "ISO-8859-1"
  | `ISO_8859_2 -> "ISO-8859-2"
  | `ISO_8859_3 -> "ISO-8859-3"
  | `ISO_8859_4 -> "ISO-8859-4"
  | `ISO_8859_5 -> "ISO-8859-5"
  | `ISO_8859_6 -> "ISO-8859-6"
  | `ISO_8859_7 -> "ISO-8859-7"
  | `ISO_8859_8 -> "ISO-8859-8"
  | `ISO_8859_9 -> "ISO-8859-9"
  | `ISO_8859_10 -> "ISO-8859-10"
  | `ISO_8859_11 -> "ISO-8859-11"
  | `ISO_8859_13 -> "ISO-8859-13"
  | `ISO_8859_14 -> "ISO-8859-14"
  | `ISO_8859_15 -> "ISO-8859-15"
  | `ISO_8859_16 -> "ISO-8859-16"

let table = function
  | `ISO_8859_1 -> ISO_8859_1.map
  | `ISO_8859_2 -> ISO_8859_2.map
  | `ISO_8859_3 -> ISO_8859_3.map
  | `ISO_8859_4 -> ISO_8859_4.map
  | `ISO_8859_5 -> ISO_8859_5.map
  | `ISO_8859_6 -> ISO_8859_6.map
  | `ISO_8859_7 -> ISO_8859_7.map
  | `ISO_8859_8 -> ISO_8859_8.map
  | `ISO_8859_9 -> ISO_8859_9.map
  | `ISO_8859_10 -> ISO_8859_10.map
  | `ISO_8859_11 -> ISO_8859_11.map
  | `ISO_8859_13 -> ISO_8859_13.map
  | `ISO_8859_14 -> ISO_8859_14.map
  | `ISO_8859_15 -> ISO_8859_15.map
  | `ISO_8859_16 -> ISO_8859_16.map

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

let rec decode_iso_8859 decoder =
  let rem = i_rem decoder in
  if rem <= 0 then if rem < 0 then `End else refill decode_iso_8859 decoder
  else
    let off = decoder.i_off in
    let pos = decoder.i_pos in
    decoder.i_pos <- decoder.i_pos + 1 ;
    ret decode_iso_8859 (r decoder.kind decoder.i off pos) 1 decoder

let pp_iso_8859 _decoder v = (v :> decode)

let decoder kind src =
  let pp = pp_iso_8859 in
  let k = decode_iso_8859 in
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
