
open MUTF8

(* Utilities *)

(* Adjust to avoid codepoints in the forbidden surrogate-pair range *)
let ucs32mk =
  List.map (fun f -> if f < 0xD800 then f else (f + 0x800))

(* Utility to print a sequence of codepoints *)
let p16s s =
  print_endline (s |> Seq.map (fun f -> BatPrintf.sprintf "%04X" f) |> List.of_seq |> String.concat " ");
  s

(* Crowbar tests *)

let utf16rt x =
  let xlen = List.length x in
  let m = (x |> List.to_seq |> of_utf16_seq) in
  Crowbar.check_eq x (m |> to_utf16_seq |> List.of_seq);
  Crowbar.check_eq xlen (utf16_length m);
  Crowbar.check_eq xlen (BatEnum.count @@ to_utf16_enum m);
  Crowbar.check_eq x (BatList.of_enum @@ to_utf16_enum m);
  Crowbar.check_eq m (m |> to_bytes |> of_bytes);
  Crowbar.check_eq x (m |> to_bytes |> of_bytes |> to_utf16_seq |> List.of_seq)

let () =
  Crowbar.add_test ~name:"utf16roundtrip" [ Crowbar.list (Crowbar.range 0xFFFF) ] utf16rt

let ucs32rt x =
  let us = ucs32mk x in
  let ulen = List.length us in
  let fmt_by_mutf8 = (us |> List.to_seq |> Seq.map (BatUChar.chr) |> of_uchar_seq) in
  Crowbar.check_eq ulen (unicode_length fmt_by_mutf8);
  let rt_by_bytes = (fmt_by_mutf8 |> to_bytes |> of_bytes) in
  Crowbar.check_eq fmt_by_mutf8 rt_by_bytes;
  Crowbar.check_eq ulen (unicode_length rt_by_bytes);
  Crowbar.check_eq us (rt_by_bytes |> to_utf16_seq |> strict_to_ucs32 |> Seq.map (BatUChar.code) |> List.of_seq);
  let fmt_by_utf8 =
    let bb = BatUTF8.Buf.create 1 in
    List.iter (fun f -> BatUTF8.Buf.add_char bb (BatUChar.chr f)) us;
    BatUTF8.Buf.contents bb
  in
  Crowbar.check_eq fmt_by_utf8 (to_utf8 fmt_by_mutf8);
  Crowbar.check_eq fmt_by_mutf8 (fmt_by_utf8 |> of_utf8);
  Crowbar.check_eq (to_bytes fmt_by_mutf8) (fmt_by_utf8 |> of_utf8 |> to_bytes)

let () =
  Crowbar.add_test ~name:"ucs32roundtrip" [ Crowbar.list (Crowbar.range 0x10F7FF) ] ucs32rt
