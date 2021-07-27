let io_buffer_size = 65536
let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt
let invalid_encode () = invalid_arg "Expected `Await encode"

let invalid_bounds off len =
  invalid_arg "Invalid bounds (off: %d, len: %d)" off len

let malformed source off pos len =
  `Malformed (Bytes.sub_string source (off + pos) len)

let unsafe_byte source off pos = Bytes.unsafe_get source (off + pos)
let unsafe_blit = Bytes.unsafe_blit
let unsafe_chr = Char.unsafe_chr
let unsafe_set_chr source off chr = Bytes.unsafe_set source off chr

(* Base character decoders. They assume enough data. *)

let r_repr source off len =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  (* assert (l = 3); *)
  let a = unsafe_byte source off 1 in
  let b = unsafe_byte source off 2 in
  let of_hex = function
    | '0' .. '9' as chr -> Char.code chr - Char.code '0'
    | 'A' .. 'F' as chr -> Char.code chr - Char.code 'A' + 10
    | _ -> assert false
  in
  (* (General 8bit representation) Any octet, except a CR or LF that is part of
     a CRLF line break of the canonical (standard) form of the data being
     encoded, may be represented by an "=" followed by a two digit hexadecimal
     representation of the octet's value. The digits of the hexadecimal
     alphabet, for this purpose, are "0123456789ABCDEF". Uppercase letters must
     be used; lowercase letters are not allowed. Thus, for example, the decimal
     value 12 (US-ASCII form feed) can be represented by "=0C", and the decimal
     value 61 (US- ASCII EQUAL SIGN) can be represented by "=3D". This rule
     must be followed except when the following rules allow an alternative
     encoding.

     See RFC2045 § 6.7. *)
  match (unsafe_byte source off 0, a, b) with
  | '=', ('0' .. '9' | 'A' .. 'F'), ('0' .. '9' | 'A' .. 'F') ->
      `Repr ((of_hex a * 16) + of_hex b)
  | '=', '\r', '\n' -> `Soft_line_break
  | _e, _a, _b -> malformed source off 0 len

let r_chr chr = `Chr chr
let r_wsp wsp = `Wsp wsp

let r_line_break source off len =
  (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
  (* assert (l = 2); *)
  match Bytes.sub_string source off len with
  | "\r\n" -> `Line_break
  | _str ->
    malformed source off 0 len

type src = [`Channel of in_channel | `String of string | `Manual]

type decode =
  [`Await | `End | `Malformed of string | `Line of string | `Data of string]

type input =
  [ `Malformed of string
  | `Soft_line_break
  | `Line_break
  | `Wsp of char
  | `Repr of int
  | `Chr of char
  | `End ]

(* [quoted-printable] has two kind to break a line but only one is relevant:
   [`Line_break]. [`Soft_line_break] must be used if longer lines are to be
   encoded with the quoted-printable encoding.

   This provides a mechanism with which long lines are encoded in such a way as
   to be restored by the user agent. The 76 character limit does not count the
   trailing CRLF, but counts all other characters, including any equal signs.

   [`Wsp] must not be represented at the end of the encoded line. We keep a
   different buffer to store them and decide if they are followed by a
   printable character (like "="), we decoded them as printable whitespaces.

   [`Repr] is a decoded 8 bits value.

   [`Chr] is only a printable character. *)

type decoder =
  { src: src
  ; mutable i: Bytes.t
  ; mutable i_off: int
  ; mutable i_pos: int
  ; mutable i_len: int
  ; t: Buffer.t
  ; w: Buffer.t
  ; h: Bytes.t
  ; mutable h_len: int
  ; mutable h_need: int
  ; mutable unsafe: bool
  ; mutable byte_count: int
  ; mutable limit_count: int
  ; mutable pp: decoder -> input -> decode
  ; mutable k: decoder -> decode }

(* On decodes that overlap two (or more) [d.i] buffers, we use [t_fill] to copy
   the input data to [d.t] and decode from there. If the [d.i] buffers are not
   too small this is faster than continuation based byte per byte writes.

   End of input is sgnaled by [d.i_pos = 0] and [d.i_len = min_int] which
   implies that [i_rem d < 0] is [true]. *)

let i_rem decoder = decoder.i_len - decoder.i_pos + 1

let end_of_input decoder =
  decoder.i <- Bytes.empty ;
  decoder.i_off <- 0 ;
  decoder.i_pos <- 0 ;
  decoder.i_len <- min_int

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

let dangerous decoder v = decoder.unsafe <- v
let reset decoder = decoder.limit_count <- 0

let ret k v byte_count decoder =
  decoder.k <- k ;
  decoder.byte_count <- decoder.byte_count + byte_count ;
  decoder.limit_count <- decoder.limit_count + byte_count ;
  if decoder.limit_count > 78 then dangerous decoder true ;
  decoder.pp decoder v

let malformed_line source off len decoder =
  Buffer.add_buffer decoder.t decoder.w ;
  Buffer.add_subbytes decoder.t source off len ;
  let line = Buffer.contents decoder.t in
  Buffer.clear decoder.w ; Buffer.clear decoder.t ; `Malformed line

let t_need decoder need =
  decoder.h_len <- 0 ;
  decoder.h_need <- need

let rec t_fill k decoder =
  let blit decoder len =
    unsafe_blit decoder.i
      (decoder.i_off + decoder.i_pos)
      decoder.h decoder.h_len len ;
    decoder.i_pos <- decoder.i_pos + len ;
    decoder.h_len <- decoder.h_len + len
  in
  let rem = i_rem decoder in
  if rem < 0 (* end of input *) then k decoder
  else
    let need = decoder.h_need - decoder.h_len in
    if rem < need then (
      blit decoder rem ;
      refill (t_fill k) decoder )
    else ( blit decoder need ; k decoder )

let rec t_decode_quoted_printable decoder =
  if decoder.h_len < decoder.h_need then
    ret decode_quoted_printable
      (malformed_line decoder.h 0 decoder.h_len decoder)
      decoder.h_len decoder
  else
    ret decode_quoted_printable
      (r_repr decoder.h 0 decoder.h_len)
      decoder.h_len decoder

and t_decode_line_break decoder =
  if decoder.h_len < decoder.h_need then
    ret decode_quoted_printable
      (malformed_line decoder.h 0 decoder.h_len decoder)
      decoder.h_len decoder
  else
    ret decode_quoted_printable
      (r_line_break decoder.h 0 decoder.h_len)
      decoder.h_len decoder

and decode_quoted_printable decoder =
  let rem = i_rem decoder in
  if rem <= 0 then
    if rem < 0 then ret (fun _decoder -> `End) `End 0 decoder
    else refill decode_quoted_printable decoder
  else
    match unsafe_byte decoder.i decoder.i_off decoder.i_pos with
    | ('\009' | '\032') as wsp ->
        (* HT | SPACE *)
        decoder.i_pos <- decoder.i_pos + 1 ;
        ret decode_quoted_printable (r_wsp wsp) 1 decoder
    | '\013' ->
        (* CR *)
        (* TODO: optimize it! *)
        t_need decoder 2 ;
        t_fill t_decode_line_break decoder
    | '=' ->
        (* TODO: optimize it! *)
        t_need decoder 3 ;
        t_fill t_decode_quoted_printable decoder
    | ('\033' .. '\060' | '\062' .. '\126') as chr ->
        Buffer.add_buffer decoder.t decoder.w ;
        Buffer.clear decoder.w ;
        decoder.i_pos <- decoder.i_pos + 1 ;
        ret decode_quoted_printable (r_chr chr) 1 decoder
    | _chr ->
        (* XXX(dinosaure): If characters other than HT, CR, LF or octets with
           decimal values greater than 126 found in incoming quoted-printable
           data by a decoder, a robust implementation might exclude them from
           the decoded data and warn the user that illegal characters were
           discovered. See RFC2045 § 6.7. *)
        let j = decoder.i_pos in
        decoder.i_pos <- decoder.i_pos + 1 ;
        ret decode_quoted_printable
          (malformed decoder.i decoder.i_off j 1)
          1 decoder

let f_fill_byte byte decoder =
  Buffer.add_char decoder.t (unsafe_chr byte) ;
  decoder.k decoder

let f_fill_chr chr decoder =
  Buffer.add_char decoder.t chr ;
  decoder.k decoder

let pp_quoted_printable decoder = function
  | `Soft_line_break ->
      Buffer.add_buffer decoder.t decoder.w ;
      let data = Buffer.contents decoder.t in
      Buffer.clear decoder.w ;
      Buffer.clear decoder.t ;
      reset decoder ;
      `Data data
  | `Line_break ->
      let line = Buffer.contents decoder.t in
      Buffer.clear decoder.w ;
      Buffer.clear decoder.t ;
      reset decoder ;
      `Line line
  | `End ->
      Buffer.add_buffer decoder.t decoder.w ;
      let data = Buffer.contents decoder.t in
      Buffer.clear decoder.w ; Buffer.clear decoder.t ; `Data data
  | `Wsp wsp ->
      Buffer.add_char decoder.w wsp ;
      decoder.k decoder
  | `Repr byte ->
      Buffer.add_buffer decoder.t decoder.w ;
      Buffer.clear decoder.w ;
      f_fill_byte byte decoder
  | `Chr chr ->
      Buffer.add_buffer decoder.t decoder.w ;
      Buffer.clear decoder.w ;
      f_fill_chr chr decoder
  | `Malformed _ as v -> v

let decoder src =
  let pp = pp_quoted_printable in
  let k = decode_quoted_printable in
  let i, i_off, i_pos, i_len =
    match src with
    | `Manual -> (Bytes.empty, 0, 1, 0)
    | `Channel _ -> (Bytes.create io_buffer_size, 0, 1, 0)
    | `String s -> (Bytes.unsafe_of_string s, 0, 0, String.length s - 1)
  in
  { src
  ; i_off
  ; i_pos
  ; i_len
  ; i
  ; t= Buffer.create 80
  ; w= Buffer.create 80
  ; h= Bytes.create 3
  ; h_need= 0
  ; h_len= 0
  ; unsafe= false
  ; limit_count= 0
  ; byte_count= 0
  ; pp
  ; k }

let decode decoder = decoder.k decoder
let decoder_byte_count decoder = decoder.byte_count
let decoder_src decoder = decoder.src
let decoder_dangerous decoder = decoder.unsafe

module Inline = struct
  (* XXX(dinosaure): I want structural typing and row polymophism on record,
     please. *)

  type unsafe_char = char
  type decode = [`Await | `End | `Malformed of string | `Char of unsafe_char]

  type input =
    [`Malformed of string | `Wsp | `Chr of char | `Repr of int | `End]

  let r_repr source off len =
    (* assert (0 <= j && 0 <= l && j + l <= String.length s); *)
    (* assert (l = 3); *)
    let a = unsafe_byte source off 1 in
    let b = unsafe_byte source off 2 in
    let of_hex = function
      | '0' .. '9' as chr -> Char.code chr - Char.code '0'
      | 'A' .. 'F' as chr -> Char.code chr - Char.code 'A' + 10
      | 'a' .. 'f' as chr -> Char.code chr - Char.code 'a' + 10
      (* RFC 2047 says: uppercase SHOULD be used for hexadecimal digits. *)
      | _ -> assert false
    in
    match (unsafe_byte source off 0, a, b) with
    | '=', ('0' .. '9' | 'A' .. 'F' | 'a' .. 'f'), ('0' .. '9' | 'A' .. 'F' | 'a' .. 'f') ->
        `Repr ((of_hex a * 16) + of_hex b)
    | _e, _a, _b -> malformed source off 0 len

  let r_wsp = `Wsp

  type decoder =
    { src: src
    ; mutable i: Bytes.t
    ; mutable i_off: int
    ; mutable i_pos: int
    ; mutable i_len: int
    ; h: Bytes.t
    ; mutable h_len: int
    ; mutable h_need: int
    ; mutable byte_count: int
    ; mutable pp: decoder -> input -> decode
    ; mutable k: decoder -> decode }

  let i_rem decoder = decoder.i_len - decoder.i_pos + 1

  let end_of_input decoder =
    decoder.i <- Bytes.empty ;
    decoder.i_off <- 0 ;
    decoder.i_pos <- 0 ;
    decoder.i_len <- min_int

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

  let ret k v byte_count decoder =
    decoder.k <- k ;
    decoder.byte_count <- decoder.byte_count + byte_count ;
    decoder.pp decoder v

  let t_need decoder need =
    decoder.h_len <- 0 ;
    decoder.h_need <- need

  let rec t_fill k decoder =
    let blit decoder len =
      unsafe_blit decoder.i
        (decoder.i_off + decoder.i_pos)
        decoder.h decoder.h_len len ;
      decoder.i_pos <- decoder.i_pos + len ;
      decoder.h_len <- decoder.h_len + len
    in
    let rem = i_rem decoder in
    if rem < 0 (* end of input *) then k decoder
    else
      let need = decoder.h_need - decoder.h_len in
      if rem < need then (
        blit decoder rem ;
        refill (t_fill k) decoder )
      else ( blit decoder need ; k decoder )

  let rec t_decode_inline_quoted_printable decoder =
    if decoder.h_len < decoder.h_need then
      ret decode_inline_quoted_printable
        (malformed decoder.h 0 0 decoder.h_len)
        decoder.h_len decoder (* XXX(dinosaure): malformed line? *)
    else
      ret decode_inline_quoted_printable
        (r_repr decoder.h 0 decoder.h_len)
        decoder.h_len decoder

  and decode_inline_quoted_printable decoder =
    let rem = i_rem decoder in
    if rem <= 0 then
      if rem < 0 then ret (fun _decoder -> `End) `End 0 decoder
      else refill decode_inline_quoted_printable decoder
    else
      match unsafe_byte decoder.i decoder.i_off decoder.i_pos with
      | '_' ->
          decoder.i_pos <- decoder.i_pos + 1 ;
          ret decode_inline_quoted_printable r_wsp 1 decoder
      | '=' ->
          t_need decoder 3 ;
          t_fill t_decode_inline_quoted_printable decoder
      | ('\033' .. '\060' | '\062' .. '\126') as chr ->
          decoder.i_pos <- decoder.i_pos + 1 ;
          ret decode_inline_quoted_printable (r_chr chr) 1 decoder
      | _ ->
          let j = decoder.i_pos in
          decoder.i_pos <- decoder.i_pos + 1 ;
          ret decode_inline_quoted_printable
            (malformed decoder.i decoder.i_off j 1)
            1 decoder

  let pp_inline_quoted_printable _decoder = function
    | `Wsp -> `Char ' '
    | `Chr chr -> `Char chr
    | `Repr byte -> `Char (unsafe_chr byte)
    | `End -> `End
    | `Malformed _ as v -> v

  let decoder src =
    let pp = pp_inline_quoted_printable in
    let k = decode_inline_quoted_printable in
    let i, i_off, i_pos, i_len =
      match src with
      | `Manual -> (Bytes.empty, 0, 1, 0)
      | `Channel _ -> (Bytes.create io_buffer_size, 0, 1, 0)
      | `String s -> (Bytes.unsafe_of_string s, 0, 0, String.length s - 1)
    in
    { src
    ; i_off
    ; i_pos
    ; i_len
    ; i
    ; h= Bytes.create 3
    ; h_need= 0
    ; h_len= 0
    ; byte_count= 0
    ; pp
    ; k }

  let decode decoder = decoder.k decoder
  let decoder_byte_count decoder = decoder.byte_count
  let decoder_src decoder = decoder.src

  type dst = [`Channel of out_channel | `Buffer of Buffer.t | `Manual]
  type encode = [`Await | `End | `Char of unsafe_char]

  type encoder =
    { dst: dst
    ; mutable o: Bytes.t
    ; mutable o_off: int
    ; mutable o_pos: int
    ; mutable o_len: int
    ; t: Bytes.t
    ; mutable t_pos: int
    ; mutable t_len: int
    ; mutable k: encoder -> encode -> [`Ok | `Partial] }

  let o_rem encoder = encoder.o_len - encoder.o_pos + 1

  let dst encoder source off len =
    if off < 0 || len < 0 || off + len > Bytes.length source
    then invalid_bounds off len ;
    encoder.o <- source ;
    encoder.o_off <- off ;
    encoder.o_pos <- 0 ;
    encoder.o_len <- len - 1

  let dst_rem = o_rem

  let partial k encoder = function
    | `Await -> k encoder
    | `Char _ | `End -> invalid_encode ()

  let flush k encoder =
    match encoder.dst with
    | `Manual ->
      encoder.k <- partial k ;
      `Partial
    | `Channel oc ->
      output oc encoder.o encoder.o_off encoder.o_pos ;
      encoder.o_pos <- 0 ;
      k encoder
    | `Buffer b ->
      let o = Bytes.unsafe_to_string encoder.o in
      Buffer.add_substring b o encoder.o_off encoder.o_pos ;
      encoder.o_pos <- 0 ;
      k encoder

  let t_range encoder len =
    encoder.t_pos <- 0 ;
    encoder.t_len <- len

  let rec t_flush k encoder =
    let blit encoder len =
      unsafe_blit encoder.t encoder.t_pos encoder.o encoder.o_pos len ;
      encoder.o_pos <- encoder.o_pos + len ;
      encoder.t_pos <- encoder.t_pos + len
    in
    let rem = o_rem encoder in
    let len = encoder.t_len - encoder.t_pos + 1 in
    if rem < len then (
      blit encoder rem ;
      flush (t_flush k) encoder )
    else ( blit encoder len ; k encoder )

  let to_hex code =
    match Char.unsafe_chr code with
    | '\000' .. '\009' -> Char.chr (Char.code '0' + code)
    | '\010' .. '\015' -> Char.chr (Char.code 'A' + code - 10)
    | _ -> assert false

  let rec encode_quoted_printable encoder v =
    let k encoder =
      encoder.k <- encode_quoted_printable ;
      `Ok
    in
    match v with
    | `Await -> k encoder
    | `End -> flush k encoder
    | `Char chr ->
      let rem = o_rem encoder in
      if rem < 1 then
        flush (fun encoder -> encode_quoted_printable encoder v) encoder
      else match chr with
        | ' ' ->
          unsafe_set_chr encoder.o (encoder.o_off + encoder.o_pos) '_' ;
          encoder.o_pos <- encoder.o_pos + 1 ;
          k encoder
        | '\033' .. '\060' | '\062' .. '\126' ->
          unsafe_set_chr encoder.o (encoder.o_off + encoder.o_pos) chr ;
          encoder.o_pos <- encoder.o_pos + 1 ;
          k encoder
        | unsafe_chr ->
          let hi = to_hex (Char.code unsafe_chr / 16) in
          let lo = to_hex (Char.code unsafe_chr mod 16) in
          let s, j, k =
            if rem < 3 then (
              t_range encoder 3 ;
              (encoder.t, 0, t_flush k) )
            else
              let j = encoder.o_pos in
              encoder.o_pos <- encoder.o_pos + 3 ;
              (encoder.o, encoder.o_off + j, k)
          in
          unsafe_set_chr s j '=' ;
          unsafe_set_chr s (j + 1) hi ;
          unsafe_set_chr s (j + 2) lo ;
          k encoder

  let encoder dst =
    let o, o_off, o_pos, o_len =
      match dst with
      | `Manual -> (Bytes.empty, 1, 0, 0)
      | `Buffer _ | `Channel _ ->
        (Bytes.create io_buffer_size, 0, 0, io_buffer_size - 1)
    in
    { dst
    ; o_off
    ; o_pos
    ; o_len
    ; o
    ; t= Bytes.create 3
    ; t_pos= 1
    ; t_len= 0
    ; k= encode_quoted_printable }

  let encode encoder v = encoder.k encoder v
  let encoder_dst encoder = encoder.dst
end

(* Encode *)

type unsafe_char = char
type dst = [`Channel of out_channel | `Buffer of Buffer.t | `Manual]
type encode = [`Await | `End | `Char of unsafe_char | `Line_break]

type encoder =
  { dst: dst
  ; mutable o: Bytes.t
  ; mutable o_off: int
  ; mutable o_pos: int
  ; mutable o_len: int
  ; t: Bytes.t
  ; mutable t_pos: int
  ; mutable t_len: int
  ; mutable c_col: int
  ; mutable k: encoder -> encode -> [`Ok | `Partial] }

let o_rem encoder = encoder.o_len - encoder.o_pos + 1

let dst encoder source off len =
  if off < 0 || len < 0 || off + len > Bytes.length source then
    invalid_bounds off len ;
  encoder.o <- source ;
  encoder.o_off <- off ;
  encoder.o_pos <- 0 ;
  encoder.o_len <- len - 1

let dst_rem = o_rem

let partial k encoder = function
  | `Await -> k encoder
  | `Char _ | `Line_break | `End -> invalid_encode ()

let flush k encoder =
  match encoder.dst with
  | `Manual ->
      encoder.k <- partial k ;
      `Partial
  | `Channel oc ->
      output oc encoder.o encoder.o_off encoder.o_pos ;
      encoder.o_pos <- 0 ;
      k encoder
  | `Buffer b ->
      let o = Bytes.unsafe_to_string encoder.o in
      Buffer.add_substring b o encoder.o_off encoder.o_pos ;
      encoder.o_pos <- 0 ;
      k encoder

let t_range encoder len =
  encoder.t_pos <- 0 ;
  encoder.t_len <- len

let rec t_flush k encoder =
  let blit encoder len =
    unsafe_blit encoder.t encoder.t_pos encoder.o encoder.o_pos len ;
    encoder.o_pos <- encoder.o_pos + len ;
    encoder.t_pos <- encoder.t_pos + len
  in
  let rem = o_rem encoder in
  let len = encoder.t_len - encoder.t_pos + 1 in
  if rem < len then (
    blit encoder rem ;
    flush (t_flush k) encoder )
  else ( blit encoder len ; k encoder )

let to_hex code =
  match Char.unsafe_chr code with
  | '\000' .. '\009' -> Char.chr (Char.code '0' + code)
  | '\010' .. '\015' -> Char.chr (Char.code 'A' + code - 10)
  | _ -> assert false

let rec encode_quoted_printable encoder v =
  let k col_count encoder =
    encoder.c_col <- encoder.c_col + col_count ;
    encoder.k <- encode_quoted_printable ;
    `Ok
  in
  match v with
  | `Await -> k 0 encoder
  | `End -> flush (k 0) encoder
  | `Line_break ->
      let rem = o_rem encoder in
      let s, j, k =
        if rem < 2 then (
          t_range encoder 2 ;
          (encoder.t, 0, t_flush (k 2)) )
        else
          let j = encoder.o_pos in
          encoder.o_pos <- encoder.o_pos + 2 ;
          (encoder.o, encoder.o_off + j, k 2)
      in
      unsafe_set_chr s j '\r' ;
      unsafe_set_chr s (j + 1) '\n' ;
      encoder.c_col <- 0 ;
      flush k encoder
  | `Char chr -> (
      let rem = o_rem encoder in
      if rem < 1 then
        flush (fun encoder -> encode_quoted_printable encoder v) encoder
      else if encoder.c_col = 75 then
        encode_soft_line_break
          (fun encoder -> encode_quoted_printable encoder v)
          encoder
      else
        match chr with
        | '\033' .. '\060' | '\062' .. '\126' ->
            unsafe_set_chr encoder.o (encoder.o_off + encoder.o_pos) chr ;
            encoder.o_pos <- encoder.o_pos + 1 ;
            k 1 encoder
        | unsafe_chr ->
            if encoder.c_col < 73 then (
              let hi = to_hex (Char.code unsafe_chr / 16) in
              let lo = to_hex (Char.code unsafe_chr mod 16) in
              let s, j, k =
                if rem < 3 then (
                  t_range encoder 3 ;
                  (encoder.t, 0, t_flush (k 3)) )
                else
                  let j = encoder.o_pos in
                  encoder.o_pos <- encoder.o_pos + 3 ;
                  (encoder.o, encoder.o_off + j, k 3)
              in
              unsafe_set_chr s j '=' ;
              unsafe_set_chr s (j + 1) hi ;
              unsafe_set_chr s (j + 2) lo ;
              k encoder )
            else
              encode_soft_line_break
                (fun encoder -> encode_quoted_printable encoder v)
                encoder )

and encode_soft_line_break k encoder =
  let rem = o_rem encoder in
  let s, j, k =
    if rem < 3 then (
      t_range encoder 3 ;
      (encoder.t, 0, t_flush k) )
    else
      let j = encoder.o_pos in
      encoder.o_pos <- encoder.o_pos + 3 ;
      (encoder.o, encoder.o_off + j, k)
  in
  unsafe_set_chr s j '=' ;
  unsafe_set_chr s (j + 1) '\r' ;
  unsafe_set_chr s (j + 2) '\n' ;
  encoder.c_col <- 0 ;
  flush k encoder

let encoder dst =
  let o, o_off, o_pos, o_len =
    match dst with
    | `Manual -> (Bytes.empty, 1, 0, 0)
    | `Buffer _ | `Channel _ ->
        (Bytes.create io_buffer_size, 0, 0, io_buffer_size - 1)
  in
  { dst
  ; o_off
  ; o_pos
  ; o_len
  ; o
  ; t= Bytes.create 3
  ; t_pos= 1
  ; t_len= 0
  ; c_col= 0
  ; k= encode_quoted_printable }

let encode encoder v = encoder.k encoder v
let encoder_dst encoder = encoder.dst
