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
(* Bud cache with some easy heuristics *)

open Utils.Open

type t = 
  { tbl : (Hash.t, (Index.t * int ref)) Hashtbl.t 
    (* 16 words, 128bytes in 64bit arch for (Hash.t * (Index.t * int ref)) *)
  ; mutable hit : int
  ; mutable added : int
  }

type stat = { ever_hit : int ; ever_added : int }

let get_stat t = { ever_hit = t.hit ; ever_added = t.added }

let reachable_words t = Obj.reachable_words (Obj.repr t)

let empty () = 
  { tbl = Hashtbl.create 0
  ; hit = 0
  ; added = 0 
  }

let size t = Hashtbl.length t.tbl

let find_opt t h = match Hashtbl.find_opt t.tbl h with
  | None -> None
  | Some (i, cntr) -> 
      cntr := min 65536 (!cntr + 256); (* completely no logical idea *)
      t.hit <- t.hit + 1;
      Some i

let add t h i = 
  Hashtbl.replace t.tbl h (i, ref 256); (* survives 8 shrink loops at least *)
  t.added <- t.added + 1

let pp_stat ppf t = 
  Format.fprintf ppf "size=%d added=%d hit=%d ratio=%.2f"
    (size t)
    t.added t.hit (float t.hit /. float (t.added + t.hit))

(* XXX very silly. must be fixed 
   * overcleaning down to 21695 when trying to reduce to 100000
*)  
let shrink goal t =
  let sz = size t in
  if sz > goal then begin
    Log.notice "Shrinking bud_cache %d to %d" sz goal;
    let rec loop () =
      let sz = size t in
      if sz > goal then begin
        Hashtbl.filter_map_inplace (fun _h (i, cntr) ->
            let n = !cntr / 2 in
            if n = 0 then None
            else Some (i, ref n)) t.tbl;
        loop ()
      end
    in
    let (), secs = with_time loop in
    Log.notice "Shrinked bud_cache %d in %.2f secs" (size t) secs;
    let words, secs = with_time (fun () ->
        reachable_words t);
    in
    Log.notice "bud_cache words %d %.2f MB in %.2f secs" words (float words *. float Sys.word_size /. 1000_000.0)  secs;
  end
  
