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

open Lwt.Infix

module H : Hashtbl.HashedType with type t = int = struct
   type t = int
   let equal = (=)
   let hash = Hashtbl.hash
end

let mk_opt replacement overflow accounting
  : (module Ringo_lwt.Sigs.CACHE_MAP_OPT with type key = int)
  = (module Ringo_lwt.Functors.Make_opt ((val Ringo.map_maker ~replacement ~overflow ~accounting) (H)))

let test_opt (module Cache: Ringo_lwt.Sigs.CACHE_MAP_OPT with type key = int) =
   let c = Cache.create 5 in

   let () = assert (Cache.length c = 0) in
   let () = Cache.replace c 0 Lwt.return_none in
   let () = assert (Cache.length c = 0) in
   let () = Cache.replace c 0 (Lwt_main.yield () >>= fun () -> Lwt.return_none) in
   Lwt_main.yield () >>= fun () ->
   Lwt_main.yield () >>= fun () ->
   let () = assert (Cache.length c = 0) in

   let () = Cache.replace c 0 (Lwt.return_some "zero") in
   let () = assert (Cache.length c = 1) in
   begin match Cache.find_opt c 0 with
   | None -> assert false
   | Some p ->
     match Lwt.state p with
     | Lwt.Return (Some "zero") -> Lwt.return_unit
     | _ -> assert false
   end >>= fun () ->

   let () = Cache.replace c 1 (Lwt_main.yield () >>= fun () -> Lwt.return_some "one") in
   let () = assert (Cache.length c = 2) in
   begin match Cache.find_opt c 1 with
   | None -> assert false
   | Some p ->
     begin match Lwt.state p with
     | Lwt.Sleep -> ()
     | _ -> assert false
     end;
     p >>= function
     | None -> assert false
     | Some "one" -> Lwt.return_unit
     | Some _ -> assert false
   end >>= fun () ->

   Lwt.return_unit

let () = Lwt_main.run @@ test_opt @@ mk_opt FIFO Strong Sloppy
let () = Lwt_main.run @@ test_opt @@ mk_opt FIFO Strong Precise
let () = Lwt_main.run @@ test_opt @@ mk_opt FIFO Weak Sloppy
let () = Lwt_main.run @@ test_opt @@ mk_opt FIFO Weak Precise
let () = Lwt_main.run @@ test_opt @@ mk_opt LRU Strong Sloppy
let () = Lwt_main.run @@ test_opt @@ mk_opt LRU Strong Precise
let () = Lwt_main.run @@ test_opt @@ mk_opt LRU Weak Sloppy
let () = Lwt_main.run @@ test_opt @@ mk_opt LRU Weak Precise

let mk_res replacement overflow accounting
  : (module Ringo_lwt.Sigs.CACHE_MAP_RESULT with type key = int)
  = (module Ringo_lwt.Functors.Make_result ((val Ringo.map_maker ~replacement ~overflow ~accounting) (H)))

let test_res (module Cache: Ringo_lwt.Sigs.CACHE_MAP_RESULT with type key = int) =
   let c = Cache.create 5 in

   let () = assert (Cache.length c = 0) in
   let () = Cache.replace c 0 (Lwt.return_error "null") in
   let () = assert (Cache.length c = 0) in
   let () = Cache.replace c 0 (Lwt_main.yield () >>= fun () -> Lwt.return_error "null") in
   Lwt_main.yield () >>= fun () ->
   Lwt_main.yield () >>= fun () ->
   let () = assert (Cache.length c = 0) in

   let () = Cache.replace c 0 (Lwt.return_ok "zero") in
   let () = assert (Cache.length c = 1) in
   begin match Cache.find_opt c 0 with
   | None -> assert false
   | Some p ->
     match Lwt.state p with
     | Lwt.Return (Ok "zero") -> Lwt.return_unit
     | _ -> assert false
   end >>= fun () ->

   let () = Cache.replace c 1 (Lwt_main.yield () >>= fun () -> Lwt.return_ok "one") in
   let () = assert (Cache.length c = 2) in
   begin match Cache.find_opt c 1 with
   | None -> assert false
   | Some p ->
     begin match Lwt.state p with
     | Lwt.Sleep -> ()
     | _ -> assert false
     end;
     p >>= function
     | Error _ -> assert false
     | Ok "one" -> Lwt.return_unit
     | Ok _ -> assert false
   end >>= fun () ->

   Lwt.return_unit

let () = Lwt_main.run @@ test_res @@ mk_res FIFO Strong Sloppy
let () = Lwt_main.run @@ test_res @@ mk_res FIFO Strong Precise
let () = Lwt_main.run @@ test_res @@ mk_res FIFO Weak Sloppy
let () = Lwt_main.run @@ test_res @@ mk_res FIFO Weak Precise
let () = Lwt_main.run @@ test_res @@ mk_res LRU Strong Sloppy
let () = Lwt_main.run @@ test_res @@ mk_res LRU Strong Precise
let () = Lwt_main.run @@ test_res @@ mk_res LRU Weak Sloppy
let () = Lwt_main.run @@ test_res @@ mk_res LRU Weak Precise
