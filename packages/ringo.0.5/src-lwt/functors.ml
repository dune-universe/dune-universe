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
  match state p with
  | Fail _ | Return _ -> p
  | Sleep ->
     let q, wk = task () in
     on_cancel q (fun () -> cancel p);
     on_any p (wakeup wk) (wakeup_exn wk);
     q

module Make (C: Ringo.CACHE_MAP)
: Sigs.CACHE_MAP with type key = C.key
= struct

  type key = C.key
  type 'a t = 'a Lwt.t C.t

  let create n = C.create n

  let remove c k =
     match C.find_opt c k with
     | None -> ()
     | Some p ->
         match Lwt.state p with
         | Fail _ -> assert false
         | Sleep -> Lwt.cancel p
         | Return _ -> C.remove c k

  let remove_if_physically_equal c k p =
     (* This is used in [replace] to guard against the following scenario:
        - [k] is bound to [p] in [c],
        - other bindings are added,
        - [p] being weakly held is popped off,
        - [p] is rejected,
        - a different binding is removed instead of [k]to[p]
     *)
     match C.find_opt c k with
     | None -> ()
     | Some pp ->
           if p == pp then
              C.remove c k
           else
              ()

  let replace c (k : key) (p: 'a Lwt.t) : unit =
     remove c k;
     match Lwt.state p with
     | Fail _ -> ()
     | Return _ ->
         C.replace c k p
     | Sleep ->
         let p = cancelable p in
         C.replace c k p;
         Lwt.on_failure p
           (fun _ -> remove_if_physically_equal c k p)

  let fold f c init =
     let open Lwt.Infix in
     C.fold
       (fun k p acc ->
          acc >>= fun acc ->
          Lwt.try_bind
            (fun () -> p)
            (fun v -> f k v acc)
            (fun _ -> Lwt.return acc))
       c
       (Lwt.return init)

  let fold_promises f c init = C.fold f c init

  let find_opt c k = C.find_opt c k

  let find_or_replace c k f =
     match C.find_opt c k with
     | Some p -> p
     | None ->
           let p = Lwt.apply f k in
           replace c k p;
           p

  let length c = C.length c

  let capacity c = C.capacity c

  let clear c =
    C.fold_v (fun p () -> Lwt.cancel p) c ();
    C.clear c

end

module Make_opt (C: Ringo.CACHE_MAP)
: Sigs.CACHE_MAP_OPT with type key = C.key
= struct

  type key = C.key
  type 'a t = 'a option Lwt.t C.t

  let create n = C.create n

  let remove c k =
     match C.find_opt c k with
     | None -> ()
     | Some p ->
         match Lwt.state p with
         | Fail _ | Return None -> assert false
         | Sleep -> Lwt.cancel p
         | Return (Some _) -> C.remove c k

  let remove_if_physically_equal c k p =
     match C.find_opt c k with
     | None -> ()
     | Some pp ->
           if p == pp then
              C.remove c k
           else
              ()

  let replace c (k : key) (p: 'a Lwt.t) : unit =
     remove c k;
     match Lwt.state p with
     | Fail _ | Return None -> ()
     | Return (Some _) ->
         C.replace c k p
     | Sleep ->
         let p = cancelable p in
         C.replace c k p;
         Lwt.on_any p
           (function
              | Some _ -> ()
              | None -> remove_if_physically_equal c k p)
           (fun _ -> remove_if_physically_equal c k p)

  let fold f c init =
     let open Lwt.Infix in
     C.fold
       (fun k p acc ->
          acc >>= fun acc ->
          Lwt.try_bind
            (fun () -> p)
            (function
               | None -> Lwt.return acc
               | Some v -> f k v acc)
            (fun _ -> Lwt.return acc))
       c
       (Lwt.return init)

  let fold_promises f c init = C.fold f c init

  let find_opt c k = C.find_opt c k

  let find_or_replace c k f =
     match C.find_opt c k with
     | Some p -> p
     | None ->
           let p = Lwt.apply f k in
           replace c k p;
           p

  let length c = C.length c

  let capacity c = C.capacity c

  let clear c =
    C.fold_v (fun p () -> Lwt.cancel p) c ();
    C.clear c

end

module Make_result (C: Ringo.CACHE_MAP)
: Sigs.CACHE_MAP_RESULT with type key = C.key
= struct

  type key = C.key
  type ('a, 'err) t = ('a, 'err) result Lwt.t C.t

  let create n = C.create n

  let remove c k =
     match C.find_opt c k with
     | None -> ()
     | Some p ->
         match Lwt.state p with
         | Fail _ | Return (Error _) -> assert false
         | Sleep -> Lwt.cancel p
         | Return (Ok _) -> C.remove c k

  let remove_if_physically_equal c k p =
     match C.find_opt c k with
     | None -> ()
     | Some pp ->
           if p == pp then
              C.remove c k
           else
              ()

  let replace c (k : key) (p: 'a Lwt.t) : unit =
     remove c k;
     match Lwt.state p with
     | Fail _ | Return (Error _) -> ()
     | Return (Ok _) ->
         C.replace c k p
     | Sleep ->
         let p = cancelable p in
         C.replace c k p;
         Lwt.on_any p
           (function
              | Ok _ -> ()
              | Error _ -> remove_if_physically_equal c k p)
           (fun _ -> remove_if_physically_equal c k p)

  let fold f c init =
     let open Lwt.Infix in
     C.fold
       (fun k p acc ->
          acc >>= fun acc ->
          Lwt.try_bind
            (fun () -> p)
            (function
               | Error _ -> Lwt.return acc
               | Ok v -> f k v acc)
            (fun _ -> Lwt.return acc))
       c
       (Lwt.return init)

  let fold_promises f c init = C.fold f c init

  let find_opt c k = C.find_opt c k

  let find_or_replace c k f =
     match C.find_opt c k with
     | Some p -> p
     | None ->
           let p = Lwt.apply f k in
           replace c k p;
           p

  let length c = C.length c

  let capacity c = C.capacity c

  let clear c =
    C.fold_v (fun p () -> Lwt.cancel p) c ();
    C.clear c

end
