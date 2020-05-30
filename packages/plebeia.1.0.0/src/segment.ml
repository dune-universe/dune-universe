(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Arthur Breitman <arthur.breitman+nospam@tezos.com>     *)
(* Copyright (c) 2019 DaiLambda, Inc. <contact@dailambda.jp>                 *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Utils.Open

let max_short_segment_length = 215 (*  = 27 * 8 - 1 *)

let max_length = 1815
  (* The limit is determined by the Node_hash.(^^)
     The max len of the second arg of (^^) is 255 chars.
     The base hash is 28 chars.
     255 - 28 = 227 chars for a segment encoding
     227 * 8 - 1 = 1815  (1 for the start bit)
  *)

type side = Left | Right

let string_of_side = function
  | Left -> "L"
  | Right -> "R"

type segment =
  | List of side list
  | Cons of side * segment
  | Append of segment * segment
  | Encoding of int (* len *) * string

type t = segment

let empty = List []

let of_sides l = List l
let of_sides = of_sides

let rec is_empty = function
  | List [] -> true
  | List _ -> false
  | Cons _ -> false
  | Append (seg1, seg2) -> is_empty seg1 && is_empty seg2
  | Encoding (0, _) -> true
  | Encoding _ -> false

let rec length = function
  | List ss -> List.length ss
  | Cons (_, seg) -> length seg + 1
  | Append (seg1, seg2) -> length seg1 + length seg2
  | Encoding (len, _) -> len

let cons x xs = Cons (x, xs)

let append seg1 seg2 = match seg1, seg2 with
  | List xs1, List xs2 -> List (xs1 @ xs2)
  | _ ->
      if is_empty seg1 then seg2
      else if is_empty seg2 then seg1
      else Append (seg1, seg2)

let rec concat = function
  | [] -> List []
  | seg::segs -> append seg @@ concat segs

(* 00....1 <--- segment bits ---> *)
let decode_ s =
  let sides_of_char c =
    let c = Char.code c in
    let f x = if c land x = 0 then Left else Right in
    [f 128; f 64; f 32; f 16; f 8; f 4; f 2; f 1]
  in
  let sides = 
    List.concat @@ List.of_seq @@ Seq.map (fun c -> sides_of_char c) @@ String.to_seq s
  in
  let rec remove_header n = function
    | Left :: xs -> 
        let n = n + 1 in
        (* assert (n < 8); *) (* we do not care how many 0s at the head *)
        remove_header n xs
    | Right :: xs -> xs
    | [] -> assert false
  in
  remove_header 0 sides

let rec to_sides seg =
  match seg with
  | List ss -> ss
  | Cons (s, seg) -> s :: to_sides seg
  | Append (seg1, seg2) -> to_sides seg1 @ to_sides seg2
  | Encoding (_, s) -> decode_ s

let rec equal seg1 seg2 =
  seg1 == seg2 ||
  match seg1, seg2 with
  | List ss1, List ss2 -> ss1 = ss2
  | Cons (s1, seg1), Cons (s2, seg2) ->
      s1 = s2 && equal seg1 seg2
  | Encoding (_, s), Encoding (_, s') -> s = s'
  | _ -> to_sides seg1 = to_sides seg2

let compare seg1 seg2 = compare (to_sides seg1) (to_sides seg2)

let rec cut = function
  | List [] -> None
  | List (x::xs) -> Some (x, List xs)
  | Cons (x,seg) -> Some (x, seg)
  | Append (seg1, seg2) when is_empty seg1 -> cut seg2
  | Append (seg1, seg2) ->
      begin match cut seg1 with
      | None -> assert false
      | Some (side, seg1) when is_empty seg1 -> Some (side, seg2)
      | Some (side, seg1) -> Some (side, Append (seg1, seg2))
      end
  | Encoding (0, _) -> None
  | Encoding (_, s) ->
      begin match decode_ s with
      | [] -> assert false
      | s::seg -> Some (s, List seg)
      end

let decode s = 
  let len = String.length s in
  let rec skip_zeros i = 
    if i >= len then assert false
    else
      let c = String.unsafe_get s i in
      if c = '\x00' then skip_zeros (i+1)
      else i
  in
  let nz = skip_zeros 0 in
  assert (len - nz > 0);
  let s = String.sub s nz (len - nz) in
  let c = Char.code @@ String.unsafe_get s 0 in
  let first_one = 
    if c land 128 <> 0 then 0
    else if c land 64 <> 0 then 1
    else if c land 32 <> 0 then 2
    else if c land 16 <> 0 then 3
    else if c land 8 <> 0 then 4
    else if c land 4 <> 0 then 5
    else if c land 2 <> 0 then 6
    else 7
  in
  let seglen = (len - nz) * 8 - first_one - 1 in
  Encoding (seglen, s)

(* 00....1 <--- segment bits ---> *)
let encode_ sides =
  let len = List.length sides in
  let bytes = len / 8 + 1 in
  let head_zero_bits = bytes * 8 - len - 1 in
  assert (0 <= head_zero_bits && head_zero_bits <= 7);
  let bytes = Bytes.create bytes in
  let make_byte = function
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: seg ->
        let bit = function
          | Left -> 0
          | Right -> 1
        in
        let byte =
          (bit x1) lsl 7
          + (bit x2) lsl 6
          + (bit x3) lsl 5
          + (bit x4) lsl 4
          + (bit x5) lsl 3
          + (bit x6) lsl 2
          + (bit x7) lsl 1
          + (bit x8) * 1
        in
        (Char.chr byte, seg)
    | seg -> Log.fatal "make_byte: %d" (List.length seg);  assert false
  in
  let rec fill_bytes byte_pos = function
    | [] ->
        Bytes.to_string bytes
    | seg ->
        let (c, seg) = make_byte seg in
        Bytes.unsafe_set bytes byte_pos c;
        let byte_pos' = byte_pos + 1 in
        fill_bytes byte_pos' seg
  in
  let sides' = 
    List.init head_zero_bits (fun _ -> Left)
    @ Right :: sides (* XXX inefficient *)
  in        
  fill_bytes 0 sides'

let encode = function
  | Encoding (_, s) -> s
  | x -> encode_ @@ to_sides x
  
let to_string s = String.concat "" (List.map string_of_side @@ to_sides s)
let string_of_sides ss = String.concat "" (List.map string_of_side ss)
let string_of_sides = string_of_sides

let of_string s =
  let rec aux st = function
    | -1 -> Some (of_sides st)
    | n ->
        match String.unsafe_get s n with
        | 'L' -> aux (Left :: st) (n-1)
        | 'R' -> aux (Right :: st) (n-1)
        | _ -> None
  in
  aux [] @@ String.length s - 1

let rec common_prefix seg1 seg2 = match (cut seg1, cut seg2) with
  | (Some (h1, t1), Some (h2, t2)) ->
    if h1 = h2 then
      let (prefix, r1, r2) = common_prefix t1 t2 in
      (h1 :: prefix, r1, r2)
    else
      ([], seg1, seg2)
  | (None, _) -> ([], empty, seg2)
  | (_, None) -> ([], seg1, empty)

let common_prefix seg1 seg2 =
  let sides, seg2, seg3 = common_prefix seg1 seg2 in
  of_sides sides, seg2, seg3

(*
let decode s =
  if String.length s = 28 then
    let len = length_of_encoded s in
    Encoding (len, s)
  else
    let len = length_of_big_encoded s in
    BigEncoding (len, s)
*)

let encoding =
  let open Data_encoding in
  string
  |> conv to_string (fun s -> from_Some (of_string s))

let segments_to_string segs = String.concat "/" @@ List.map to_string segs (* XXX make it deprecated *)
let string_of_segments = segments_to_string

let of_char c =
  let c = Char.code c in
  let bit n = if c land n = 0 then Left else Right in
  [ bit 128 ; bit 64 ; bit 32 ; bit 16 ; bit 8 ; bit 4 ; bit 2 ; bit 1]

let encode_string s =
  let open Data_encoding in
  match Binary.to_bytes Data_encoding.Encoding.string s with
  | Error _ -> assert false
  | Ok b -> 
      let of_binary_string s =
        let rec f st = function
          | -1 -> st
          | i ->
              let c = String.unsafe_get s i in
              f (of_char c @ st) (i-1)
        in
        f [] (String.length s - 1)
      in
      of_sides @@ of_binary_string (Bytes.to_string b)

let decode_string seg =
  let sides = to_sides seg in
  let buf = Buffer.create 10 in
  let bit n = function
    | Left -> 0
    | Right -> n
  in
  let rec f = function
    | [] -> Some (Buffer.contents buf)
    | b7::b6::b5::b4::b3::b2::b1::b0::sides ->
        Buffer.add_char buf
          @@ Char.chr @@ bit 128 b7
                       + bit 64 b6
                       + bit 32 b5
                       + bit 16 b4
                       + bit 8 b3
                       + bit 4 b2
                       + bit 2 b1
                       + bit 1 b0;
          f sides
    | _ -> None
  in
  match f sides with
  | None -> None
  | Some s ->
      match Data_encoding.Binary.of_bytes Data_encoding.Encoding.string (Bytes.of_string s) with
      | Error _ -> None
      | Ok x -> Some x

module Segs = struct
  (* growing segments at the end *)
  type t =
    { rev_segs : segment list
    ; bottom : side list (* reversed! *)
    }

  let empty = { rev_segs = []; bottom = [] }
  let add_side t side = { t with bottom = side :: t.bottom }
  let append_seg t seg = { t with bottom = List.rev_append (to_sides seg) t.bottom }
  let append_sides t sides = { t with bottom = List.rev_append sides t.bottom }
  let append_rev_sides t rev_sides = { t with bottom = rev_sides @ t.bottom }
  let push_bud t =
    (* root bud never changes the segs *)
    if t.bottom = [] && t.rev_segs = [] then t
    else begin
      assert ( t.bottom <> [] );
      { rev_segs = of_sides (List.rev t.bottom) :: t.rev_segs; bottom= [] }
    end
  let finalize t = List.rev (of_sides (List.rev t.bottom) :: t.rev_segs)

  let to_string t =
    let segs = finalize t in
    String.concat "/" (List.map to_string segs)

  let of_segments segs = match List.rev segs with
    | [] -> empty
    | bottom :: rev_segs -> { rev_segs; bottom= to_sides bottom }
                            
  let last t = List.rev t.bottom
end

