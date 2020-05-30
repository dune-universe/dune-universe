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
(* 
  Caching of small Value.t
   
  Strategy:
   
  * Value.t larger than max_leaf_size is never cached.

  * Any Value.t equal to or smaller than max_leaf_size is cached for small
    amount of time.
  * Each Value.t in the cache has its score.  Score decays gradually.
  * Once its score becomes 0, the Value.t may be removed from the cache.

  * If cache hits, the Value.t gains some score.
*)
open Utils
open Utils.Open

type Error.t += Hashcons of string
let () = Error.register_printer @@ function
  | Hashcons s -> Some ("Hashcons: " ^ s)
  | _ -> None

type config = 
  { max_leaf_size : int 
  ; max_words : int
  }

let default_config = 
  { max_leaf_size= 36 
  ; max_words = 100_000_000 / Sys.word_size (* 100MB *)
  }

(* Number of blocks for the value of the given size in bytes *)
let blocks size =
  if size <= 32 then 2
  else if size <= 64 then 3
  else 
    (* assuming using only 1 chunk *)
    (size + 6 + 31) / 32

type contents = { index: Index.t; mutable freq: int }

type t = 
  { tbl : (Value.t, contents) Hashtbl.t
  ; config : config
  ; mutable current_words : int
  ; mutable saved_blocks : int
  ; by_size : int array
  }

let config t = t.config

let create config =
  { tbl = Hashtbl.create 101
  ; config
  ; current_words= 0
  ; saved_blocks= 0
  ; by_size= Array.make config.max_leaf_size 0
  }

let stat ppf t =
  Format.fprintf ppf "Phash saved: %d  saved: %.2f MB  current_words: %d  max: %d@." 
    t.saved_blocks (float t.saved_blocks *. float Sys.word_size /. 1000000.0)
    t.current_words
    t.config.max_words

let stat_table ppf t =
  Array.iteri (fun i n ->
      Format.fprintf ppf "Phash bucket %d %d@." (i+1) n) t.by_size

let find t v =
  let len = Value.length v in
  if len = 0 || len > t.config.max_leaf_size then 
    Error (Hashcons "hashcons: too large or 0")
  else
    match Hashtbl.find_opt t.tbl v with
    | None -> Ok None
    | Some c -> 
        c.freq <- min 10000 (c.freq + 100);
        t.saved_blocks <- t.saved_blocks + blocks len;
        Ok (Some c.index)

let entry_words v = Value.length v / 8 + 1 + 16 (* 16 words per Hashtbl.entry excluding the Value *)

let score v { freq; _ } =
  let len = Value.length v in
  let blocks = blocks len in
  let gain = blocks * freq in
  let cost = entry_words v in
  gain / cost

let may_shrink t =
  if t.current_words <= t.config.max_words then ()
  else begin
    let down_to = t.config.max_words * 9 / 10 in
    Log.notice "Phash: shrinking from %d to %d" t.current_words down_to;
    Format.kasprintf (Log.notice "%s") "%a" stat t;
    Format.kasprintf (Log.notice "%s") "%a" stat_table t;
    let (), secs = with_time @@ fun () ->
      let xs = 
        List.sort (fun (_,s1,_) (_,s2,_) -> compare s2 s1)
        @@ Hashtbl.fold (fun v contents acc -> (v, score v contents, contents) :: acc) t.tbl []
      in
      ignore @@ List.fold_left (fun acc (v, _, c) ->
          let cost = entry_words v in
          let acc' = acc - cost in
          let acc' = if acc' < 0 then 0 else acc' in
          if acc' > 0 then begin 
            c.freq <- c.freq * 9 / 10;
          end else begin 
            Hashtbl.remove t.tbl v;
            t.current_words <- t.current_words - (entry_words v);
            let i = Value.length v - 1 in
            Array.unsafe_set t.by_size i (Array.unsafe_get t.by_size i - 1)
          end;
          acc') down_to xs
    in
    Log.notice "Phash: shrank to %d in %.2f secs" t.current_words secs;
    Format.kasprintf (Log.notice "%s") "%a" stat t;
    Format.kasprintf (Log.notice "%s") "%a" stat_table t
  end

let add t v index =
  let len = Value.length v in
  if len = 0 || len > t.config.max_leaf_size then 
    Error (Hashcons "hashcons: too large or 0")
  else begin
    match Hashtbl.find_opt t.tbl v with
    | Some { index=index'; freq } ->
        (* Let's use newer index *)
        Hashtbl.replace t.tbl v { index= max index index'; freq };
        Ok ()
    | None ->
        Hashtbl.replace t.tbl v { index; freq= 100 };
        t.current_words <- t.current_words + entry_words v;
        Array.unsafe_set t.by_size (len-1) @@ Array.unsafe_get t.by_size (len-1) + 1;
        Ok ()
  end
