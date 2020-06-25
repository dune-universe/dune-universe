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

(** The caches in this package are Lwt-friendly versions of the caches in Ringo. 
    The documentation of this package assumes you are familiar with Ringo. *)

module type CACHE_MAP = sig

   (** A Mutable structure akin to a [Ringo.CACHE_MAP] but with Lwt-aware
       functions. E.g., consider the following use of a [Ringo.CACHE_MAP]:

       [let c = Ringo_map.create 1024 in
        let resolve k =
           match Ringo_map.find_opt k with
           | Some v -> Lwt.return v
           | None ->
                 do_resolve k >>= fun v ->
                 Ringo_map.replace c k v;
                 Lwt.return v]

       In this example, there is a race condition: if [do_resolve] takes time to
       complete, another call to [resolve] may be made concurrently to the first
       one.

       The function [find_or_replace] in [Ringo_lwt.CACHE_MAP] works around this
       issue. *)

  (** The type of keys on which values in the cache are indexed. *)
  type key

  (** The type of Lwt-friendly caches holding bindings from [key] to ['a].

      Instead of adding values directly to this cache, you can add promises
      (using [replace]) or, more interestingly, atomically (a) querying for an
      already bound promises or (b) generating a new one if needed. This helps
      avoid race conditions.

      A promise is removed from the cache if:
      - The cache overflows (in which case, the removal of the promise depends
        on the policies of the cache, see {!Ringo.CACHE_MAP} for details).
      - The promise is still held by the cache when
          - the cache is [clear]ed (in which case the promise is canceled).
          - it is [replace]d by another one (in which case it is canceled).
          - it is explicitly [remove]d (in which case it is canceled).
          - it is rejected.

      If a promise is not held by the cache, then it cannot be removed from the
      cache and it will not be canceled by the cache. *)
  type 'a t

  (** [create n] creates a cache with a size-bound of [n]. Remember that the
      size-bound is not upheld strictly by all caches. *)
  val create : int -> 'a t

  (** [replace c k p] binds the key [k] to [p] in the cache [c].

      Note that when a promise is rejected, it is automatically removed from the
      cache.

      Note that, for the purpose of determining if an inserted binding is
      supernumerary, and thus if it pushes another binding out of the cache, an
      unresolved binding counts fully. *)
  val replace : 'a t -> key -> 'a Lwt.t -> unit

  (** [fold f c init] folds the function [f] and value [init] over the bindings
      of [c]. More specifically, it takes the bindings that are in [c] at the
      moment of the call (inserting a binding whilst the [fold] promise is
      pending has no effect on the [fold] promise) and traverses them
      sequentially: it waits for one step of the folding to resolve before
      starting the next one. Promises that are rejected are not visible by this
      [fold] operation: they are simply ignored.

      E.g., you can run [fold (fun _ _ () -> Lwt.return_unit) ()] to wait for
      all currently-held bindings to resolve.

      Note that for some caches, this function may fold over a subset of the
      bindings of [c]. Specifically, on caches with a [Weak] overflow policy,
      only the strongly-held elements are folded over. *)
  val fold : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  (** [fold_promises f c init] folds the function [f] and value [init] over the
      promises of bindings of [c]. More specifically, it takes the bindings that
      are in [c] at the moment of the call (inserting a binding whilst the
      [fold] promise is pending has no effect on the [fold] promise) and
      traverses them all immediately. The function that folds over the bindings
      is given the promises (rather than the values these promises resolve to).

      E.g., You can count the number of resolved/pending like so:
      [fold_promises
        (fun _ p (sleeping, not_sleeping) ->
           match Lwt.state p with
           | Sleep -> (sleeping + 1, not_sleeping)
           | Return _ -> (sleeping, not_sleeping + 1)
           | Fail _ -> assert false (* these are removed from the cache *)
         )
         c
         (0, 0)
      ]

      Note that for some caches, this function may fold over a subset of the
      bindings of [c]. Specifically, on caches with a [Weak] overflow policy,
      only the strongly-held elements are folded over. *)
  val fold_promises : (key -> 'a Lwt.t -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [find_opt c k] is [None] if [k] is not bound in [c]. Otherwise it is
      [Some p] where [p] is bound to [k] in [c].

      Note that the in some caches, this may have a side effect on the
      [k]-to-[v] binding. Specifically, in some caches, it might make it less
      likely to be removed when supernumerary bindings are inserted. *)
  val find_opt : 'a t -> key -> 'a Lwt.t option

  (** [find_or_replace c k f] behaves likes [find_opt c k] if [k] is bound in
      [c], and it behaves like [replace c k f] otherwise. Either way, it returns
      the promise that resolves to the value associated to [k] whichever
      behavior [find_or_replace] resembled. *)
  val find_or_replace : 'a t -> key -> (key -> 'a Lwt.t) -> 'a Lwt.t

  (** [remove c k] removes the binding from [k] in [c]. If [k] is not bound in
      [c], it does nothing. If the binding is not resolved yet, it also cancels
      the promise.

      Note that in some caches, removed bindings can still count towards the
      size bound for some time. *)
  val remove : 'a t -> key -> unit

  (** [length c] is the number of bindings held by [c]. *)
  val length : 'a t -> int

  (** [capacity c] is the number of bindings [c] can hold:
      [capacity (create n) = n] *)
  val capacity : 'a t -> int

  (** [clear c] removes all bindings from [c]. It also cancels unresolved
      bindings. *)
  val clear : 'a t -> unit

end

module type CACHE_MAP_OPT = sig

  (** This is similar to [CACHE_MAP] except that it handles [option].
      Specifically, you can insert ['a option Lwt.t] and promises that are
      fulfilled with [None] are treated like promises that are rejected:

      - They are removed from the cache automatically.
      - They are not folded over by [fold].

    *)

  type key

  type 'a t

  val create : int -> 'a t

  val replace : 'a t -> key -> 'a option Lwt.t -> unit

  val fold : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  val fold_promises : (key -> 'a option Lwt.t -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val find_opt : 'a t -> key -> 'a option Lwt.t option

  val find_or_replace : 'a t -> key -> (key -> 'a option Lwt.t) -> 'a option Lwt.t

  val remove : 'a t -> key -> unit

  val length : 'a t -> int

  val capacity : 'a t -> int

  val clear : 'a t -> unit

end

module type CACHE_MAP_RESULT = sig

  (** This is similar to [CACHE_MAP] except that it handles [result].
      Specifically, you can insert [('a, 'b) result Lwt.t] and promises that
      are fulfilled with [Error _] are treated like promises that are rejected:

      - They are removed from the cache automatically.
      - They are not folded over by [fold].
    *)

  type key

  type ('a, 'err) t

  val create : int -> ('a, 'err) t

  val replace : ('a, 'err) t -> key -> ('a, 'err) result Lwt.t -> unit

  val fold : (key -> 'a -> 'b -> 'b Lwt.t) -> ('a, 'err) t -> 'b -> 'b Lwt.t

  val fold_promises : (key -> ('a, 'err) result Lwt.t -> 'b -> 'b) -> ('a, 'err) t -> 'b -> 'b

  val find_opt : ('a, 'err) t -> key -> ('a, 'err) result Lwt.t option

  val find_or_replace : ('a, 'err) t -> key -> (key -> ('a, 'err) result Lwt.t) -> ('a, 'err) result Lwt.t

  val remove : ('a, 'err) t -> key -> unit

  val length : ('a, 'err) t -> int

  val capacity : ('a, 'err) t -> int

  val clear : ('a, 'err) t -> unit

end
