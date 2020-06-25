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
   let c = Cache.create 50 in

   (* Cancel on remove *)
   let (wt0, _) = Lwt.task () in
   let () = Cache.replace c 0 wt0 in
   let () = Cache.remove c 0 in
   let () =
      match Lwt.state wt0 with
      | Lwt.Fail Lwt.Canceled -> ()
      | Lwt.Fail _ -> assert false
      | Lwt.Return _ -> assert false
      | Lwt.Sleep -> assert false
   in

   (* Cancel on clear *)
   let (wt1, _) = Lwt.task () in
   let (wt2, _) = Lwt.task () in
   let (wt3, _) = Lwt.task () in
   let (wt4, wk4) = Lwt.task () in
   let never _ = fst @@ Lwt.wait () in

   let () = Cache.replace c 1 wt1 in
   let () = for k = 4 to 7 do Cache.replace c k (never ()) done in
   let () = Cache.replace c 2 wt2 in
   let () = for k = 8 to 15 do Cache.replace c k (never())  done in
   let () = Cache.replace c 3 wt3 in
   let () = Cache.replace c 4 wt4 in
   let () = Lwt.wakeup wk4 "BOO" in

   let () = Cache.clear c in
   let () =
      match Lwt.state wt1 with
      | Lwt.Return _ | Lwt.Sleep -> assert false
      | Lwt.Fail Lwt.Canceled -> ()
      | Lwt.Fail _ -> assert false
   in
   let () =
      match Lwt.state wt2 with
      | Lwt.Return _ | Lwt.Sleep -> assert false
      | Lwt.Fail Lwt.Canceled -> ()
      | Lwt.Fail _ -> assert false
   in
   let () =
      match Lwt.state wt3 with
      | Lwt.Return _ | Lwt.Sleep -> assert false
      | Lwt.Fail Lwt.Canceled -> ()
      | Lwt.Fail _ -> assert false
   in
   let () =
      match Lwt.state wt4 with
      | Lwt.Return "BOO" -> ()
      | Lwt.Return _ | Lwt.Sleep | Lwt.Fail _ -> assert false
   in

   Lwt.return_unit

let () = Lwt_main.run @@ test @@ mk LRU Strong Sloppy
let () = Lwt_main.run @@ test @@ mk LRU Strong Precise
let () = Lwt_main.run @@ test @@ mk LRU Weak Sloppy
let () = Lwt_main.run @@ test @@ mk LRU Weak Precise
let () = Lwt_main.run @@ test @@ mk FIFO Strong Sloppy
let () = Lwt_main.run @@ test @@ mk FIFO Strong Precise
let () = Lwt_main.run @@ test @@ mk FIFO Weak Sloppy
let () = Lwt_main.run @@ test @@ mk FIFO Weak Precise
