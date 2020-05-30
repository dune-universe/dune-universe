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
let deep_stat level c =
  let open Node in
  let bud_empty = ref 0 in
  let bud_non_empty = ref 0 in
  let leaf = ref 0 in
  let internal = ref 0 in
  let extender = ref 0 in
  let cntr = ref 0 in
  (* XXX A shared node is visited more than once *)
  Cursor.fold ~init:() c (fun () c ->
      incr cntr;
      if !cntr mod 1000000 = 0 then begin
        Log.notice "stat: %d steps" !cntr; (* like 8580000 steps *)
      end;
      let _, v = Cursor.view c in
      begin match v with
        | Bud (None, _, _) -> incr bud_empty
        | Bud (Some _, _, _) -> incr bud_non_empty
        | Leaf (_v, _, _) -> incr leaf
        | Internal (_, _, _, _) -> incr internal
        | Extender (_, _, _, _) -> incr extender
      end;
      `Continue, ());

  let fd = Unix.(openfile "stats.csv" [O_CREAT; O_WRONLY; O_APPEND] 0o644) in
  let s = Printf.sprintf "%ld, %d, %d, %d, %d, %d\n" level
      !bud_empty
      !bud_non_empty
      !internal
      !extender
      !leaf
  in
  prerr_endline "stat done.";
  let w = Unix.single_write fd (Bytes.of_string s) 0 (String.length s) in
  assert (w > 0);
  Unix.close fd
