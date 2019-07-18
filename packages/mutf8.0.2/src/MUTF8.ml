
open Seq

type t = {
    (* The MUTF8-encoded string *)
    mutf8: string;
    (* Number of UTF-16 chars it decodes to *)
    utf16len: int;
    (* Number of surrogate pairs (not including unpaired surrogates);
       in other words this is the difference between the UTF16 length
       of the string and the UCS32 length of the string *)
    surrogates: int;
    (* The number of NULs (codepoint 0) in the string *)
    nuls: int;
    (* Whether the string contains any unpaired surrogate codepoints *)
    has_unpaired: bool;
  }

let debugdump t =
  let hexof b =
    (BatString.to_list b) |> List.map (fun f -> BatPrintf.sprintf "%02X" (Char.code f)) |> BatString.concat " "
  in
  BatPrintf.printf "--\nrepr: %S\nhex: %s\nu16len: %d (%d surrogates, %d nuls, has-unpaired=%s)\n" t.mutf8 (hexof t.mutf8) t.utf16len t.surrogates t.nuls (if t.has_unpaired then "yes" else "no")

(* Used when tallying up UCS sequences, such as BatUChars or (real) UTF8 *)
type ucs_utf16_info = {
    mutable utf16len: int;
    mutable surrogate_count: int;
    mutable nul_count: int;
    mutable is_7bit: bool;
  }

let make_ucs_utf16_info () = {
    is_7bit = true;
    utf16len = 0;
    surrogate_count = 0;
    nul_count = 0;
  }

let ucs_utf16_tally info uch =
  let codepoint = BatUChar.code uch in
  if codepoint > 0xFFFF then
    begin
      (* Surrogate pair. *)
      info.utf16len <- 2 + info.utf16len;
      info.surrogate_count <- 1 + info.surrogate_count;
      info.is_7bit <- false;
    end
  else
    begin
      (* This utf8 sequence fits in a single utf16 character *)
      info.utf16len <- 1 + info.utf16len;

      if codepoint > 127 || codepoint = 0 then
        info.is_7bit <- false; (* But not in a single byte *)

      if codepoint = 0 then
        info.nul_count <- 1 + info.nul_count; (* Weird MUTF8 rule for NULs *)
    end

let finish_tally info bytes =
  { mutf8 = bytes;
    utf16len = info.utf16len;
    surrogates = info.surrogate_count;
    nuls = info.nul_count;
    has_unpaired = false; }

(* Used when tallying up UTF16 sequences which may not be valid
   encodings of unicode sequences, such as raw u16 sequences, or MUTF8 *)
type wobbly_utf16_info = {
    utf16len: int;
    surrogate_count: int;
    unpaired_count: int;
    nul_count: int;
    prev_was_high: bool;
  }

let wobbly_info_zero = {
    utf16len = 0;
    surrogate_count = 0;
    unpaired_count = 0;
    nul_count = 0;
    prev_was_high = false;
  }

let wobbly_tally {unpaired_count; utf16len; surrogate_count; nul_count; prev_was_high} utf16 =
  let c' = succ utf16len in
  if utf16 >= 0xDC00 && utf16 < 0xE000 then
    (* The second half of a surrogate pair *)
    if prev_was_high then
      (* ... which followed the first half *)
      {unpaired_count; utf16len = c'; surrogate_count = (succ surrogate_count);
       nul_count; prev_was_high = false }
    else
      (* ... which did not follow the first half *)
      {unpaired_count = (succ unpaired_count); utf16len = c'; surrogate_count;
       nul_count; prev_was_high = false }
  else
    let upc' = (if prev_was_high then succ unpaired_count else unpaired_count) in
    if utf16 >= 0xD800 && utf16 < 0xDC00 then
      {unpaired_count = upc'; utf16len = c'; surrogate_count; nul_count; prev_was_high = true }
    else if utf16 = 0 then
      {unpaired_count = upc'; utf16len = c'; surrogate_count;
       nul_count = (succ nul_count); prev_was_high = false }
    else
      {unpaired_count = upc'; utf16len = c'; surrogate_count;
       nul_count; prev_was_high = false }

let wobbly_finish info buf =
  { mutf8 = buf;
    utf16len = info.utf16len;
    surrogates = info.surrogate_count;
    nuls = info.nul_count;
    has_unpaired = info.prev_was_high || (info.unpaired_count > 0);
  }


let mutf8_append buf codepoint =
  if codepoint >= 1 && codepoint <= 127 then
    Buffer.add_char buf (Char.chr codepoint)
  else if codepoint <= 0x07FF then
    begin
      (* Note that codepoint 0 also falls into this branch,
         unlike normal CESU or UTF *)
      Buffer.add_char buf (Char.chr ((codepoint asr 6) lor 0xC0));
      Buffer.add_char buf (Char.chr ((codepoint land 0x3F) lor 0x80));
    end
  else if codepoint <= 0xFFFF then
    begin
      (* Dalvik's MUTF8 is "wobbly", in the sense that unpaired surrogates get
         passed through, and aren't considered encoding errors. *)
      Buffer.add_char buf (Char.chr ((codepoint asr 12) lor 0xE0));
      Buffer.add_char buf (Char.chr (((codepoint asr 6) land 0x3F) lor 0x80));
      Buffer.add_char buf (Char.chr ((codepoint land 0x3F) lor 0x80));
    end
  else if codepoint <= 0x10FFFF then
    begin
      (* All higher codepoints get encoded as surrogate pairs. This means
         we convert the codepoint into two UTF-16 values and then convert
         those into UTF-8 sequences individually. *)
      let h = (codepoint asr 10) - 64 in
      Buffer.add_char buf '\xED';
      Buffer.add_char buf (Char.chr ((h asr 6) lor 0xA0));
      Buffer.add_char buf (Char.chr ((h land 0x3F) lor 0x80));
      Buffer.add_char buf '\xED';
      Buffer.add_char buf (Char.chr (((codepoint asr 6) land 0x0F) lor 0xB0));
      Buffer.add_char buf (Char.chr ((codepoint land 0x3F) lor 0x80));
    end
  else
    raise BatUChar.Out_of_range

let code_at utf8 pos =
  let rec multibyte p v n =
    let p' = succ p in
    let ch = Char.code (String.get utf8 p') in
    if ch land 0xC0 <> 0x80 then
      raise BatUTF8.Malformed_code
    else
      let v' = (v lsl 6) lor (ch land 0x3F) in
      let n' = pred n in
      if n' = 0 then
        (v', succ p')
      else
        multibyte p' v' n'
  in
  try
    let ch = Char.code (String.get utf8 pos) in
    if ch < 128 then
      (ch, succ pos)
    else if (ch land 0xE0) = 0xC0 then
      multibyte pos (ch land 0x1F) 1
    else if (ch land 0xF0) = 0xE0 then
      multibyte pos (ch land 0x0F) 2
    else if (ch land 0xF8) = 0xF0 then
      multibyte pos (ch land 0x07) 3
    else if (ch land 0xFC) = 0xF8 then
      multibyte pos (ch land 0x03) 4
    else
      raise BatUTF8.Malformed_code
  with Invalid_argument _ -> raise BatUTF8.Malformed_code

let join_surrogate h l =
  0x10000 +
    ( ( h land 0x3FF ) lsl 10 ) lor
      ( l land 0x3FF )

let ucs_codepoint_at utf8 pos =
  let cp = code_at utf8 pos in
  let (v, p') = cp in
  if v >= 0xD800 && v < 0xDC00 && pos < (String.length utf8) then
    let (sv, sp') = code_at utf8 p' in
    if sv >= 0xDC00 && sv < 0xE000 then
      (join_surrogate v sv, sp')
    else
      cp
  else
    cp

let seq_of_utf8 ?(startbyte=0) utf8 =
  let len = String.length utf8 in
  let rec next_node pos =
    if pos = len then
      Nil
    else
      try
        let (value, nextpos) = code_at utf8 pos in
        Cons (value, fun () -> next_node nextpos)
      with Invalid_argument _ ->
        raise BatUTF8.Malformed_code
  in
  (fun () -> next_node startbyte)

let rec wobbly_to_ucs32' n =
  match n with
  | Nil -> Nil
  | Cons (v, rest) ->
     if v < 0xD800 || v >= 0xDC00 then
       Cons (v, wobbly_to_ucs32 rest)
     else
       match rest () with
       | Nil -> Cons (v, fun () -> Nil)
       | Cons (s2, rest') when s2 >= 0xDC00 && s2 < 0xE000 ->
          Cons (join_surrogate v s2, wobbly_to_ucs32 rest' )
       | Cons (_, _) ->
          Cons (v, wobbly_to_ucs32 rest)
and wobbly_to_ucs32 s = (fun () -> wobbly_to_ucs32' (s ()))

let rec strict_to_ucs32' n =
  match n with
  | Nil -> Nil
  | Cons (v, rest) ->
     if v < 0xD800 || v >= 0xE000 then
       Cons (BatUChar.chr v, strict_to_ucs32 rest)
     else if v > 0xDC00 then
       raise BatUChar.Out_of_range
     else
       match rest () with
       | Nil -> raise BatUChar.Out_of_range
       | Cons (s2, rest') when s2 >= 0xDC00 && s2 < 0xE000 ->
          Cons (BatUChar.chr (join_surrogate v s2), strict_to_ucs32 rest' )
       | _ -> raise BatUChar.Out_of_range
and strict_to_ucs32 s = (fun () -> strict_to_ucs32' (s ()))

let of_utf8 s =
  let info = make_ucs_utf16_info () in
  BatUTF8.iter (ucs_utf16_tally info) s;
  if info.surrogate_count = 0 && info.nul_count = 0 then
    (* The most common case: the MUTF8 encoding is the same as the UTF8 encoding *)
    { mutf8 = s; utf16len = info.utf16len;
      surrogates = 0; nuls = 0; has_unpaired = false; }
  else
    begin
      let buf = Buffer.create ((String.length s) + (2 * info.surrogate_count) + info.nul_count) in
      BatUTF8.iter (fun ch -> mutf8_append buf (BatUChar.code ch)) s;
      finish_tally info (Buffer.contents buf)
    end


(* ********************************************************************** *)
(* Conversion functions                                                   *)
(* ********************************************************************** *)


let to_utf8 { mutf8 = buf; utf16len = _; surrogates; nuls; has_unpaired = _; } =
  if surrogates = 0 && nuls = 0 then
    buf
  else
    let utf8 = Buffer.create (String.length buf) in
    let rec scan p =
      if p < String.length buf then
        let (value, p') = ucs_codepoint_at buf p in
        BatUTF8.Buf.add_char utf8 (BatUChar.chr value);
        scan p'
    in
    scan 0;
    Buffer.contents utf8

let of_uchar_seq s =
  let info = make_ucs_utf16_info () in
  let buf = Buffer.create 32 in
  let rec convert s' =
    match s' () with
    | Nil -> finish_tally info (Buffer.contents buf)
    | Cons (uch, rest) ->
       begin
         mutf8_append buf (BatUChar.code uch);
         ucs_utf16_tally info uch;
         convert rest
       end
  in
  convert s

let of_utf16_seq s =
  let buf = Buffer.create 32 in
  let rec convert i s' =
    match s' () with
    | Nil -> wobbly_finish i (Buffer.contents buf)
    | Cons (utf16, rest) ->
       if utf16 < 0 || utf16 > 0xFFFF then
         raise BatUChar.Out_of_range
       else
         begin
           mutf8_append buf utf16;
           convert (wobbly_tally i utf16) rest
         end
  in convert wobbly_info_zero s

let to_utf16_seq s =
  seq_of_utf8 s.mutf8

let to_utf16_enum s =
  let rec make_enum pos =
    let cursor = ref pos in
    BatEnum.make
      ~next:  (fun () -> let bytepos, utfpos = !cursor in
                         if bytepos >= String.length s.mutf8 then
                           raise BatEnum.No_more_elements;
                         let value, nextpos = code_at s.mutf8 bytepos in
                         cursor := (nextpos, succ utfpos);
                         value)
      ~count: (fun () -> s.utf16len - (snd !cursor))
      ~clone: (fun () -> make_enum !cursor)
  in
  make_enum (0, 0)

let to_bytes r = r.mutf8

let of_bytes buf =
  let len = String.length buf in
  let rec scan i p =
    if p <> len then
      if String.get buf p = '\x00' then
        raise BatUTF8.Malformed_code
      else
        let (value, p') = code_at buf p in
        if value > 0xFFFF then
          raise BatUTF8.Malformed_code
        else
          scan (wobbly_tally i value) p'
    else
      wobbly_finish i buf
  in
  scan wobbly_info_zero 0

;;

let utf16_length (r: t) = r.utf16len

let unicode_length (r: t) = r.utf16len - r.surrogates

let compare a b =
  let mcmp = String.compare a.mutf8 b.mutf8 in
  if mcmp = 0 || (a.nuls = 0 && b.nuls = 0) then
    mcmp
  else
    let alen = String.length a.mutf8 in
    let blen = String.length b.mutf8 in
    let rec compare' apos bpos =
      if apos = alen then
        if bpos = blen then
          0 (* Not reachable for well-formed MUTF8, due to String.compare fast path *)
        else
          -1
      else if bpos = blen then
        1
      else
        let (ach, apos') = code_at a.mutf8 apos in
        let (bch, bpos') = code_at b.mutf8 bpos in
        if ach < bch then
          -1
        else if ach > bch then
          1
        else
          compare' apos' bpos'
    in
    compare' 0 0
