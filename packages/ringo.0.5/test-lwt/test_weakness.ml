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

(* NOTE:
   This test is intended to be run manually. It is not part of a test-suite, it
   is not on the CI, it doesn't run with [dune runtest]. This is because the
   test is fragile and does not fit well in the yes-no tests of a CI.

   To run the test, use the command
   [dune build @test-lwt/runleaktest]. This dune target invokes the present
   file; it also sets up some environment variables so that the OCaml heap stays
   small.

   To interpret the output of the test, refer to the comments below in this
   file.

*)

open Lwt.Infix

module H : Hashtbl.HashedType with type t = int list = struct
  type t = int list
  let equal l1 l2 = match (l1, l2) with ([x], [y]) -> x = y | _ -> assert false
  let hash = Hashtbl.hash
end

let mk replacement accounting : (module Ringo_lwt.Sigs.CACHE_MAP with type key = int list) =
   (module Ringo_lwt.Functors.Make
              ((val Ringo.map_maker ~replacement ~overflow:Weak ~accounting) (H)))


let () = Random.self_init ()

let test_weakness name (module Cache: Ringo_lwt.Sigs.CACHE_MAP with type key = int list) =
   let c = Cache.create 16 in

   let size_of () = Obj.(reachable_words (repr c)) in

   let len () = Cache.length c in

   let module Spamming = struct
     let n = ref (Random.bits ())

     (* We retain references to some keys to make sure that it does not cause a
        leak. *)
     let retainer = ref []

     let spam global_stop : unit =
       match Random.int 2000 with
       | r when r <= 200 ->
         let k = !n - Random.int 56 in
         Cache.remove c [k]
       | r when 201 <= r && r <= 225 ->
         incr n ;
         let k = [!n] in
         let _ : char Lwt.t = Cache.find_or_replace c k
           (fun _ -> Lwt.choose [global_stop ; fst @@ Lwt.task ()]) in
         if r <= 202 then retainer := k :: !retainer
       | r when 226 <= r && r <= 250 ->
         incr n ;
         let k = [!n] in
         let _ : char Lwt.t = Cache.find_or_replace c k
           (fun _ -> Lwt.choose [global_stop ; fst @@ Lwt.wait ()]) in
         if r <= 227 then retainer := k :: !retainer
       | r when 251 <= r && r <= 275 ->
         incr n ;
         let k = [!n] in
         let _ : char Lwt.t =
           Cache.find_or_replace c k
             (fun _ ->
                Lwt_unix.sleep (Random.float 0.02) >>= fun () ->
                Lwt.fail Exit) in
         if r <= 253 then retainer := k :: !retainer
       | r when 276 <= r && r <= 1999 ->
         incr n ;
         let k = [!n] in
         let _ : char Lwt.t =
           Cache.find_or_replace c k
             (fun _ ->
              Lwt_unix.sleep (Random.float 0.02) >>= fun () ->
              Lwt.return (string_of_int !n).[0]) in
         if r <= 300 then retainer := k :: !retainer
       | _ -> assert false

   end in

   let rec run max_sz max_len n limit stop stopper =
     if n > limit then (
       Lwt.wakeup stopper 'x';
       Cache.fold (fun _ _ () -> Lwt.return_unit) c ()
     ) else (
       begin if n mod 25_000 = 0 then (
         Lwt_unix.sleep 0.01 >>= fun () ->
         (* Every so many insert operations, some statistics are printed.
            The most important of these statistics is the size of the
            data-structure (in words). Or more precisely, the maximum of all the
            sizes that have been observed so far. If there are no leaks, this
            number should plateau.
            Another statistic that's printed is the maximum number of elements ever
            observed inside the data-structure. It should also plateau. *)
         Format.printf
           "steps: %8d;        max size:     %8d words;        max len:     %8d \
            elements;        retained:  %8d\n\
            %!"
           n
           max_sz
           max_len
           (List.length !Spamming.retainer) ;
         Lwt.return_unit
       ) else (
         Lwt.return_unit
       ) end >>= fun () ->
       Spamming.spam stop ;
       Lwt.pause () >>= fun () ->
       run (max max_sz (size_of ())) (max max_len (len ())) (n + 1) limit stop stopper ) in

   let rec run_with_gc n limit stop stopper =
     if n > limit then (
       Lwt.wakeup stopper 'x';
       Cache.fold (fun _ _ () -> Lwt.return_unit) c ()
     ) else (
       begin if n mod 25_000 = 0 then (
         Lwt_unix.sleep 0.01 >>= fun () ->
         Gc.full_major () ;
         Format.printf
           "steps: %8d;        post-GC size: %8d words;        post-GC len: %8d \
            elements;        retained:  %8d\n\
            %!"
           n
           (size_of ())
           (len ())
           (List.length !Spamming.retainer) ;
         Lwt.return_unit
       ) else (
         Lwt.return_unit
       ) end >>= fun () ->
       Spamming.spam stop ;
       Lwt.pause () >>= fun () ->
       run_with_gc (n + 1) limit stop stopper ) in

   let () = Format.printf "START %s\n%!" name in

   let stop, stopper = Lwt.task () in
   let () = Lwt_main.run @@ run 0 0 0 750_000 stop stopper in

   let () = Format.printf "WITH_GC\n%!" in

   let stop, stopper = Lwt.task () in
   let () = Lwt_main.run @@ run_with_gc 750_000 1_000_000 stop stopper in

   let () = Format.printf "END %s\n%!" name in

   ()

let () = test_weakness "LRU/Sloppy" @@ mk LRU Sloppy
let () = test_weakness "LRU/Precise" @@ mk LRU Precise
let () = test_weakness "FIFO/Sloppy" @@ mk FIFO Sloppy
let () = test_weakness "FIFO/Precise" @@ mk FIFO Precise
