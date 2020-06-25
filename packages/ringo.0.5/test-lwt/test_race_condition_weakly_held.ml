(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module H : Hashtbl.HashedType with type t = int = struct
   type t = int
   let equal = (=)
   let hash = Hashtbl.hash
end

let mk replacement overflow accounting : (module Ringo_lwt.Sigs.CACHE_MAP with type key = int) =
   (module Ringo_lwt.Functors.Make
              ((val Ringo.map_maker ~replacement ~overflow ~accounting) (H)))

let test (module Cache: Ringo_lwt.Sigs.CACHE_MAP with type key = int) =
   (* create a small cache *)
   let c = Cache.create 5 in

   (* add single promise *)
   let (wtalpha, wkalpha) = Lwt.task () in
   let () = Cache.replace c 0 wtalpha in

   (* overflow the cache *)
   let never () = fst @@ Lwt.task () in
   let () = Cache.replace c 1 @@ never () in
   let () = Cache.replace c 2 @@ never () in
   let () = Cache.replace c 3 @@ never () in
   let () = Cache.replace c 4 @@ never () in
   let () = Cache.replace c 5 @@ never () in
   let () = Cache.replace c 6 @@ never () in
   let () = Cache.replace c 7 @@ never () in
   let () = Cache.replace c 8 @@ never () in
   let () = Cache.replace c 9 @@ never () in

   (* assert that the original promise has gone *)
   let () = assert (Option.is_none @@ Cache.find_opt c 0) in

   (* insert new promise for the same binding *)
   let (wtomega, _) = Lwt.task () in
   let () = Cache.replace c 0 wtomega in

   (* reject old promise *)
   let () = Lwt.wakeup_exn wkalpha (Failure "doh") in

   (* new promise is not removed *)
   let () = assert (Option.is_some @@ Cache.find_opt c 0) in
   let () = match Lwt.state wtalpha with
     | Fail (Failure _) -> ()
     | Sleep -> assert false
     | Return _ -> assert false
     | Fail _ -> assert false
   in
   let () = match Lwt.state wtomega with
     | Sleep -> ()
     | Return _ -> assert false
     | Fail _ -> assert false
   in

   Lwt.return_unit

let () = Lwt_main.run @@ test @@ mk FIFO Strong Sloppy
let () = Lwt_main.run @@ test @@ mk FIFO Strong Precise
let () = Lwt_main.run @@ test @@ mk FIFO Weak Sloppy
let () = Lwt_main.run @@ test @@ mk FIFO Weak Precise
let () = Lwt_main.run @@ test @@ mk LRU Strong Sloppy
let () = Lwt_main.run @@ test @@ mk LRU Strong Precise
let () = Lwt_main.run @@ test @@ mk LRU Weak Sloppy
let () = Lwt_main.run @@ test @@ mk LRU Weak Precise
