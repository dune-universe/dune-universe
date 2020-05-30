(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
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
open Hash
open Node

type t = Hash.t * string

let encoding =
  let open Data_encoding in
  tup2 Hash.encoding string
  |> conv (fun (h,s) -> (h,s)) (fun (h,s) -> h,s)

let to_string (s, s') = Hash.to_string s ^ s'

let to_hex_string t = let `Hex s = Hex.of_string @@ to_string t in s

let prefix = fst

let split s =
  Hash.of_string (String.sub s 0 28),
  String.sub s 28 (String.length s - 28)
    
let h n s = hash_list [ String.make 1 (Char.chr n); s ]

let (||) s1 s2 = 
  let s1 = to_string s1 in
  s1 ^ s2

let (^^) s1 s2 = 
  let s1 = to_string s1 in
  let l2 = String.length s2 in
  assert (0 < l2 && l2 < 256);
  let c = Char.chr l2 in
  s1 ^ s2 ^ String.make 1 c

let of_bud = function
  | None -> (Hash.zero, "")
  | Some hash -> (set_last_2bits 3 @@ h 2 @@ to_string hash, "")

let of_internal l r = (set_last_2bits 0 @@ h 1 (l ^^ to_string r), "")

let of_leaf v = (h 0 @@ Value.to_string v, "")

let of_extender seg h = 
  assert (snd h = "");
  split (h || Segment.encode seg)

let compute context node : (view * t) =
  let rec aux : node -> (view * t) = function
    | Disk (index, wit) ->
        let v, nh = aux' (load_node context index wit) in v, nh
    | View v ->
        let v, nh = aux' v in v, nh

  and aux' : view -> (view * t) = fun v ->
    match v with
    (* easy case where it's already hashed *)
    | Leaf (_, _, Hashed h)
    | Bud (_, _, Hashed h)
    | Internal (_, _, _, Hashed h)  -> (v, (h, ""))
    | Extender (seg, _, _, Hashed h) -> (v, of_extender seg (h,""))

    (* from here, hashing is necessary *)

    | Leaf (v, _, Not_Hashed) ->
        let h = of_leaf v in
        (_Leaf (v, Not_Indexed, Hashed (prefix h)), h)

    | Bud (Some underneath, _, Not_Hashed) ->
        let (v, h) = aux underneath in
        let h = of_bud @@ Some h in
        (_Bud (Some (View v), Not_Indexed, Hashed (prefix h)), h)

    | Bud (None, _, Not_Hashed) ->
        let h = Hash.zero, "" in
        (_Bud (None, Not_Indexed, Hashed (prefix h)), h)

    | Internal (left, right, _, Not_Hashed) ->
        let (left, hl) = aux left
        and (right, hr) = aux right in
        let h = of_internal hl hr in
        (_Internal (View left, View right, Not_Indexed, Hashed (prefix h)), h)

    | Extender (segment, underneath, _, Not_Hashed)  ->
        let (underneath, h0) = aux underneath in
        assert (snd h0 = "");
        let h = of_extender segment h0 in
        (_Extender (segment, View underneath, Not_Indexed, Hashed (prefix h)), h)
  in
  aux node
