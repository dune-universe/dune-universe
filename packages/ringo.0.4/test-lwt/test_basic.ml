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

let mk replacement overflow accounting : (module Ringo_lwt.Sigs.CACHE_MAP with type key = int) =
   (module Ringo_lwt.Functors.Make
              ((val Ringo.map_maker ~replacement ~overflow ~accounting) (H)))

let test (module Cache: Ringo_lwt.Sigs.CACHE_MAP with type key = int) =
   let c = Cache.create 5 in

   (* Basic cache functionality, without concurrency *)
   let () = assert (Cache.length c = 0) in

   begin match Cache.find_opt c 0 with | Some _ -> assert false | None -> () end;

   let () = Cache.replace c 0 (Lwt.return "zero") in
   let () = assert (Cache.length c = 1) in
   begin match Cache.find_opt c 0 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "zero" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->

   let () = Cache.replace c 1 (Lwt.return "one") in
   let () = assert (Cache.length c = 2) in
   begin match Cache.find_opt c 0 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "zero" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->
   begin match Cache.find_opt c 1 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "one" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->

   let () = Cache.replace c 1 (Lwt.return "un") in
   let () = assert (Cache.length c = 2) in
   begin match Cache.find_opt c 0 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "zero" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->
   begin match Cache.find_opt c 1 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "un" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->

   Lwt_main.yield () >>= fun () ->

   let () = Cache.remove c 1 in
   let () = assert (Cache.length c = 1) in
   begin match Cache.find_opt c 0 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "zero" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->
   begin match Cache.find_opt c 1 with | Some _ -> assert false | None -> () end;

   let () = List.iteri (fun i s -> Cache.replace c i (Lwt.return s))
      ["zilch"; "ein"; "two"; "tres"; "si"; "cinq"; "......"; "seven"] in
   let () = assert (Cache.length c >= 5) in


   (* Concurrency *)
   let f1 n = Lwt_unix.sleep 0.1 >>= fun () -> Lwt.return (string_of_int n) in
   let () = Cache.replace c 1024 (f1 1024) in
   let () = Cache.replace c 1024 (f1 1024) in
   let w1 = Cache.find_or_replace c 1024 f1 in
   let w2 = Cache.find_or_replace c 1024 f1 in
   w1 >>= fun v1 ->
   w2 >>= fun v2 ->
   let () = assert (v1 == v2) in
   let () = assert (v1 = "1024") in
   begin match Cache.find_opt c 1024 with
     | None -> assert false
     | Some p ->
         p >>= function
         | "1024" -> Lwt.return_unit
         | _ -> assert false end >>= fun () ->

   let p = Lwt_unix.sleep 0.1 >>= fun () -> Lwt.return "1025" in
   let f1 _ = p in
   let () = Cache.replace c 1025 (f1 1025) in
   let () = Cache.replace c 1025 (f1 1025) in
   let w1 = Cache.find_or_replace c 1025 f1 in
   let w2 = Cache.find_or_replace c 1025 f1 in
   Lwt.cancel p;
   begin match Cache.find_opt c 1025 with | Some _ -> assert false | None -> () end;
   let () =
      match Lwt.state w1 with
      | Lwt.Return _ | Lwt.Sleep -> assert false
      | Lwt.Fail Lwt.Canceled -> ()
      | Lwt.Fail _ -> assert false in
   let () =
      match Lwt.state w2 with
      | Lwt.Return _ | Lwt.Sleep -> assert false
      | Lwt.Fail Lwt.Canceled -> ()
      | Lwt.Fail _ -> assert false in


   (* Final *)
   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   Lwt.return_unit

let () = Lwt_main.run @@ test @@ mk FIFO Strong Sloppy
let () = Lwt_main.run @@ test @@ mk FIFO Strong Precise
let () = Lwt_main.run @@ test @@ mk FIFO Weak Sloppy
let () = Lwt_main.run @@ test @@ mk FIFO Weak Precise
let () = Lwt_main.run @@ test @@ mk LRU Strong Sloppy
let () = Lwt_main.run @@ test @@ mk LRU Strong Precise
let () = Lwt_main.run @@ test @@ mk LRU Weak Sloppy
let () = Lwt_main.run @@ test @@ mk LRU Weak Precise
