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

(* [cancelable p] is a promise [up] that is resolved when [p] is resolved. If
   [p] is cancelable, [up] is indistinguishable from [p]. If [p] is not
   cancelable then calling [Lwt.cancel up] before [p] is determined will cause
   [up] to [fail] with [Lwt.Canceled]. *)
let cancelable p =
  let open Lwt in
  let q, wk = task () in
  on_cancel q (fun () -> cancel p);
  on_any p (wakeup wk) (wakeup_exn wk);
  q

module Make (C: Ringo.CACHE_MAP)
: Sigs.CACHE_MAP with type key = C.key
= struct

  type key = C.key
  module CleanersTable = Hashtbl.Make(C.H)
  type 'a t = {cache: 'a Lwt.t C.t; cleaners: unit Lwt.t CleanersTable.t}

  let create n = {cache = C.create n; cleaners = CleanersTable.create n}

  let remove {cache; cleaners;} k =
     match C.find_opt cache k with
     | None ->
         assert (match CleanersTable.find_opt cleaners k with None -> true | Some _ -> false)
     | Some p ->
         match Lwt.state p with
         | Fail _ -> assert false
         | Sleep ->
               assert (match CleanersTable.find_opt cleaners k with Some _ -> true | None -> false);
               Lwt.cancel p
         | Return _ ->
               assert (match CleanersTable.find_opt cleaners k with None -> true | Some _ -> false);
               C.remove cache k

  let replace ({cache; cleaners} as c : 'a t) (k : key) (p: 'a Lwt.t) : unit =
     remove c k;
     match Lwt.state p with
     | Fail _ -> ()
     | Return _ ->
         C.replace cache k p
     | Sleep ->
         C.replace cache k (cancelable p);
         let cleaner =
            Lwt.try_bind (fun () -> p)
              (fun _ ->
                 CleanersTable.remove cleaners k;
                 Lwt.return_unit)
              (fun _ ->
                 C.remove cache k;
                 CleanersTable.remove cleaners k;
                 Lwt.return_unit)
         in
         CleanersTable.add cleaners k cleaner

  let fold f {cache; _} init =
     let open Lwt.Infix in
     C.fold
       (fun k p acc ->
          acc >>= fun acc ->
          p >>= fun v ->
          f k v acc)
       cache
       (Lwt.return init)

  let fold_promises f {cache; _} init = C.fold f cache init

  let find_opt {cache; _} k = C.find_opt cache k

  let find_or_replace ({cache; _} as c) k f =
     match C.find_opt cache k with
     | Some p -> p
     | None ->
           let p = f k in
           replace c k p;
           p

  let length {cache; _} = C.length cache

  let capacity {cache; _} = C.capacity cache

  let clear {cache; cleaners;} =
    CleanersTable.iter (fun _ cleaner -> Lwt.cancel cleaner) cleaners;
    C.fold_v (fun p () -> Lwt.cancel p) cache ();
    CleanersTable.clear cleaners;
    C.clear cache

end

module Make_opt (C: Ringo.CACHE_MAP)
: Sigs.CACHE_MAP_OPT with type key = C.key
= struct

  type key = C.key
  module CleanersTable = Hashtbl.Make(C.H)
  type 'a t = {cache: 'a option Lwt.t C.t; cleaners: unit Lwt.t CleanersTable.t}

  let create n = {cache = C.create n; cleaners = CleanersTable.create n}

  let remove {cache; cleaners;} k =
     match C.find_opt cache k with
     | None ->
         assert (match CleanersTable.find_opt cleaners k with None -> true | Some _ -> false)
     | Some p ->
         match Lwt.state p with
         | Fail _ -> assert false
         | Sleep ->
               assert (match CleanersTable.find_opt cleaners k with Some _ -> true | None -> false);
               Lwt.cancel p
         | Return _ ->
               assert (match CleanersTable.find_opt cleaners k with None -> true | Some _ -> false);
               C.remove cache k

  let replace ({cache; cleaners} as c : 'a t) (k : key) (p: 'a option Lwt.t) : unit =
     remove c k;
     match Lwt.state p with
     | Fail _ -> ()
     | Return None -> ()
     | Return (Some _) ->
         C.replace cache k p
     | Sleep ->
         C.replace cache k (cancelable p);
         let cleaner =
            Lwt.try_bind (fun () -> p)
              (function
                | Some _ ->
                    CleanersTable.remove cleaners k;
                    Lwt.return_unit
                | None ->
                 C.remove cache k;
                 CleanersTable.remove cleaners k;
                 Lwt.return_unit)
              (fun _ ->
                 C.remove cache k;
                 CleanersTable.remove cleaners k;
                 Lwt.return_unit)
         in
         CleanersTable.add cleaners k cleaner

  let fold f {cache; _} init =
     let open Lwt.Infix in
     C.fold
       (fun k p acc ->
          acc >>= fun acc ->
          p >>= function
          | None -> Lwt.return acc
          | Some v -> f k v acc)
       cache
       (Lwt.return init)

  let fold_promises f {cache; _} init = C.fold f cache init

  let find_opt {cache; _} k = C.find_opt cache k

  let find_or_replace ({cache; _} as c) k f =
     match C.find_opt cache k with
     | Some p -> p
     | None ->
           let p = f k in
           replace c k p;
           p

  let length {cache; _} = C.length cache

  let capacity {cache; _} = C.capacity cache

  let clear {cache; cleaners;} =
    CleanersTable.iter (fun _ cleaner -> Lwt.cancel cleaner) cleaners;
    C.fold_v (fun p () -> Lwt.cancel p) cache ();
    CleanersTable.clear cleaners;
    C.clear cache

end

module Make_result (C: Ringo.CACHE_MAP)
: Sigs.CACHE_MAP_RESULT with type key = C.key
= struct

  type key = C.key
  module CleanersTable = Hashtbl.Make(C.H)
  type ('a, 'err) t = {cache: ('a, 'err) result Lwt.t C.t; cleaners: unit Lwt.t CleanersTable.t}

  let create n = {cache = C.create n; cleaners = CleanersTable.create n}

  let remove {cache; cleaners;} k =
     match C.find_opt cache k with
     | None ->
         assert (match CleanersTable.find_opt cleaners k with None -> true | Some _ -> false)
     | Some p ->
         match Lwt.state p with
         | Fail _ -> assert false
         | Sleep ->
               assert (match CleanersTable.find_opt cleaners k with Some _ -> true | None -> false);
               Lwt.cancel p
         | Return _ ->
               assert (match CleanersTable.find_opt cleaners k with None -> true | Some _ -> false);
               C.remove cache k

  let replace ({cache; cleaners} as c : ('a, 'err) t) (k : key) (p: ('a, 'err) result Lwt.t) : unit =
     remove c k;
     match Lwt.state p with
     | Fail _ -> ()
     | Return (Error _) -> ()
     | Return (Ok _) ->
         C.replace cache k p
     | Sleep ->
         C.replace cache k (cancelable p);
         let cleaner =
            Lwt.try_bind (fun () -> p)
              (function
                | Ok _ ->
                    CleanersTable.remove cleaners k;
                    Lwt.return_unit
                | Error _ ->
                 C.remove cache k;
                 CleanersTable.remove cleaners k;
                 Lwt.return_unit)
              (fun _ ->
                 C.remove cache k;
                 CleanersTable.remove cleaners k;
                 Lwt.return_unit)
         in
         CleanersTable.add cleaners k cleaner

  let fold f {cache; _} init =
     let open Lwt.Infix in
     C.fold
       (fun k p acc ->
          acc >>= fun acc ->
          p >>= function
          | Error _ -> Lwt.return acc
          | Ok v -> f k v acc)
       cache
       (Lwt.return init)

  let fold_promises f {cache; _} init = C.fold f cache init

  let find_opt {cache; _} k = C.find_opt cache k

  let find_or_replace ({cache; _} as c) k f =
     match C.find_opt cache k with
     | Some p -> p
     | None ->
           let p = f k in
           replace c k p;
           p

  let length {cache; _} = C.length cache

  let capacity {cache; _} = C.capacity cache

  let clear {cache; cleaners;} =
    CleanersTable.iter (fun _ cleaner -> Lwt.cancel cleaner) cleaners;
    C.fold_v (fun p () -> Lwt.cancel p) cache ();
    CleanersTable.clear cleaners;
    C.clear cache

end
