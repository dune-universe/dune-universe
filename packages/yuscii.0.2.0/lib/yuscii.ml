(* XXX(dinosaure): IO_BUFFER_SIZE *)
let io_buffer_size = 65536

let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt
let invalid_bounds off len = invalid_arg "Invalid bounds (off: %d, len: %d)" off len
let pp = Format.fprintf

let b64d v =
  let inv =
    [|   -1;   -1;   -1;   -1;   -1;   -1;   -1;   -1;
         -1;   -1;   -1;   -1;   -1;   -1;   -1;   -1;
         -1;   -1;   -1;   -1;   -1;   -1;   -1;   -1;
         -1;   -1;   -1;   -1;   -1;   -1;   -1;   -1;
         -1;   -1;   -1;   -1;   -1;   -1;   -1;   -1;
         -1;   -1;   -1; 0x3e;   -1;   -1;   -1; 0x3f;
       0x34; 0x35; 0x36; 0x37; 0x38; 0x39; 0x3a; 0x3b;
       0x3c; 0x3d;   -1;   -1;   -1;   -1;   -1;   -1;
         -1; 0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06;
       0x07; 0x08; 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x0e;
       0x0f; 0x10; 0x11; 0x12; 0x13; 0x14; 0x15; 0x16;
       0x17; 0x18; 0x19;   -1;   -1;   -1;   -1;   -1;
         -1; 0x1a; 0x1b; 0x1c; 0x1d; 0x1e; 0x1f; 0x20;
       0x21; 0x22; 0x23; 0x24; 0x25; 0x26; 0x27; 0x28;
       0x29; 0x2a; 0x2b; 0x2c; 0x2d; 0x2e; 0x2f; 0x30;
       0x31; 0x32; 0x33;   -1;   -1;   -1;   -1;   -1; |] in
  if v < 128 then inv.(v) else (-1)

let is_high v =
  v >= 0xd800 && v <= 0xdbff

let is_low v =
  v >= 0xdc00 && v <= 0xdfff

let malformed source off pos len = `Malformed (Bytes.sub_string source (off + pos) len)

let unsafe_byte source off pos =
  Char.code (Bytes.unsafe_get source (off + pos))

type src = [ `Channel of in_channel | `String of string | `Manual ]
type decode = [ `Await | `End | `Malformed of string | `Uchar of Uchar.t ]

let pp_decode ppf = function
  | `Uchar u -> pp ppf "@[`Uchar U+%04X@]" (Uchar.to_int u)
  | `End -> pp ppf "`End"
  | `Await -> pp ppf "`Await"
  | `Malformed s ->
    let l = String.length s in
    pp ppf "@[`Malformed (";
    for i = 0 to l - 1 do pp ppf "%02X" (Char.code s.[i]) done;
    pp ppf ")@]"

type decoder =
  { src : src
  ; mutable i : Bytes.t
  ; mutable i_off : int
  ; mutable i_pos : int
  ; mutable i_len : int
  ; mutable acc : int
  ; mutable bits : int
  ; mutable high : int
  ; mutable f_open : bool
  ; mutable f_used : bool
  ; mutable byte_count : int
  ; mutable pp : decoder -> [ `Malformed of string | `Uchar of Uchar.t ] -> decode
  ; mutable k : decoder -> decode }

let i_rem decoder = decoder.i_len - decoder.i_pos + 1

let end_of_input decoder =
    decoder.i <- Bytes.empty
  ; decoder.i_off <- 0
  ; decoder.i_pos <- 0
  ; decoder.i_len <- min_int

let src decoder source off len =
  if (off < 0 || len < 0 || off + len > Bytes.length source)
  then invalid_bounds off len
  else if (len = 0) then end_of_input decoder
  else ( decoder.i <- source
       ; decoder.i_off <- off
       ; decoder.i_pos <- 0
       ; decoder.i_len <- len - 1)

let refill k decoder = match decoder.src with
  | `Manual -> decoder.k <- k; `Await
  | `String _ -> end_of_input decoder; k decoder
  | `Channel ic ->
    let len = input ic decoder.i 0 (Bytes.length decoder.i) in
    (src decoder decoder.i 0 len; k decoder)

let ret k v byte_count decoder =
    decoder.k <- k
  ; decoder.byte_count <- decoder.byte_count + byte_count
  ; decoder.pp decoder v

let consume k byte_count decoder =
    decoder.k <- k
  ; decoder.byte_count <- decoder.byte_count + byte_count
  ; decoder.k decoder

let finish decoder =
  if decoder.f_open || decoder.high <> 0
  then `Malformed "" (* XXX(dinosaure): other error message? *)
  else `End

let rec decode_utf_7 decoder =
  let rem = i_rem decoder in
  let malformed n =
    let i_pos = decoder.i_pos in
    decoder.i_pos <- decoder.i_pos + 1
  ; ret decode_utf_7
      (malformed decoder.i decoder.i_off i_pos n)
      n decoder in

  if rem <= 0
  then (if rem < 0 then finish decoder else refill decode_utf_7 decoder)
  else
    let byte = unsafe_byte decoder.i decoder.i_off decoder.i_pos in

    if byte < 0 || byte > 127 then malformed 1 else
      ( if decoder.f_open
        then decode_shifted_unicode byte decoder
        else if byte = 0x2b
        then
          (* begin decoding base64. *)
          ( decoder.f_open <- true
          ; decoder.f_used <- false
          ; decoder.bits <- 0
          ; decoder.i_pos <- decoder.i_pos + 1
          ; consume decode_utf_7 1 decoder )
        else
          ( let uchar byte = `Uchar (Uchar.unsafe_of_int byte) in

            (* there was unpaired high surrogate. *)
            if decoder.high <> 0 then malformed 1 else
              ( decoder.i_pos <- decoder.i_pos + 1
              ; ret decode_utf_7 (uchar byte) 1 decoder)))

and decode_shift_character decoder =
  let uchar byte = `Uchar (Uchar.unsafe_of_int byte) in
  (* Also, as a special case, the sequence "+-" may be used to encoder the
     character "+". *)
  decoder.i_pos <- decoder.i_pos + 1
; decoder.f_open <- false
; ret decode_utf_7 (uchar 0x2b) 1 decoder

and decode_shifted_unicode byte decoder =
  let uchar byte = `Uchar (Uchar.unsafe_of_int byte) in
  let malformed n =
    let i_pos = decoder.i_pos in
    decoder.i_pos <- decoder.i_pos + 1
  ; ret decode_utf_7
      (malformed decoder.i decoder.i_off i_pos n)
      n decoder in

  (* currently shift decoding. *)

  if not decoder.f_used && byte = 0x2d then decode_shift_character decoder else
    let value = b64d byte in

    if value < 0 then
      if decoder.bits >= 6 then malformed 1 else            (* too many bits in
                                                               accumulation
                                                               buffer *)
        let mask = (1 lsl decoder.bits) - 1 in
        if decoder.acc land mask <> 0 then malformed 1 else (* non-zero trailing
                                                               base64 bits *)
          ( decoder.f_open <- false
          ; if byte <> 0x2d then
              if decoder.high <> 0 then malformed 1 else    (* unpaired high
                                                               surrogate *)
              if decoder.f_used then
                ( decoder.i_pos <- decoder.i_pos + 1
                ; ret decode_utf_7 (uchar byte) 1 decoder)
              else malformed 1                              (* shift encoded
                                                               ended without
                                                               being used *)
            else
              ( decoder.i_pos <- decoder.i_pos + 1
              ; consume decode_utf_7 1 decoder))
    else
      (* accumulate more base64 bits. *)
      ( decoder.f_used <- true
      ; decoder.acc <- (decoder.acc lsl 6) lor value
      ; decoder.bits <- decoder.bits + 6
      ; if decoder.bits >= 16
        then
          (* extract a code point. *)
          ( decoder.bits <- decoder.bits - 16
          ; let code_point =
              (decoder.acc lsr decoder.bits) land 0xffff in

          if decoder.high <> 0
            then
              let low = code_point in
              let high = decoder.high in
              (* next code point must be low surrogate. *)
              ( if not (is_low low) then malformed 1 else
                  ( decoder.high <- 0
                  ; let code_point =
                      ((high - 0xd800) * 0x400)
                      + ((low - 0xdc00) + 0x10000) in
                    decoder.i_pos <- decoder.i_pos + 1
                  ; ret decode_utf_7 (uchar code_point) 1 decoder))
            else if is_high code_point
            then
              (* save and recurse to look for low surrogate. *)
              ( decoder.high <- code_point
              ; decoder.i_pos <- decoder.i_pos + 1
              ; consume decode_utf_7 1 decoder)
            else if is_low code_point then malformed 1      (* unpaired low
                                                               surrogate *)
            else
              (* not surrogate. *)
              ( decoder.i_pos <- decoder.i_pos + 1
              ; ret decode_utf_7 (uchar code_point) 1 decoder))
        else
          (* consume safely and look next. *)
          ( decoder.i_pos <- decoder.i_pos + 1
          ; consume decode_utf_7 1 decoder))

let pp_utf_7 _decoder v = (v :> decode)

let decoder src =
  let pp = pp_utf_7 in
  let k = decode_utf_7 in
  let i, i_off, i_pos, i_len = match src with
    | `Manual -> Bytes.empty, 0, 1, 0
    | `Channel _ -> Bytes.create io_buffer_size, 0, 1, 0
    | `String s -> Bytes.unsafe_of_string s, 0, 0, String.length s - 1 in
  { src
  ; i_off
  ; i_pos
  ; i_len
  ; i
  ; acc = 0
  ; bits = 0
  ; high = 0
  ; f_open = false
  ; f_used = false
  ; byte_count = 0
  ; pp
  ; k }

let decode decoder = decoder.k decoder
let decoder_byte_count decoder = decoder.byte_count
let decoder_src decoder = decoder.src

module String = struct
  type 'a folder = 'a -> int -> [ `Malformed of string | `Uchar of Uchar.t ] -> 'a

  let fold ?off ?len folder acc str =
    let off, len = match off, len with
      | Some off, Some len -> off, len
      | None, Some len -> 0, len
      | Some off, None -> off, String.length str - off
      | None, None -> 0, String.length str in
    let acc = ref acc in
    let decoder = decoder (`String (String.sub str off len)) in
    let rec go decoder = match decode decoder with
      | `Await -> assert false (* can never occur iff [src = `String _] *)
      | `End -> !acc
      | (`Uchar _ | `Malformed _) as v ->
        acc := folder !acc (decoder_byte_count decoder) v
      ; go decoder in
    go decoder
end
