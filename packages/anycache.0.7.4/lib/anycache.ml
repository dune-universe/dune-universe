(******************************************************************************)
(* Copyright (c) 2014-2016 Skylable Ltd. <info-copyright@skylable.com>        *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)
module type Monad = Types.Monad
module type S = sig
  type key
  type 'a deferred
  type 'a t
  type 'a validator = (key * 'a option) -> 'a deferred

  val create : int -> 'a t
  val with_cache : 'a t -> (key -> 'a deferred) -> key -> 'a deferred
  val with_validator : 'a t -> 'a validator -> key -> 'a deferred
  val get : 'a t -> key -> 'a option deferred
  val set : 'a t -> key -> 'a -> unit
end

module Make(K: Map.OrderedType)(M : Monad) = struct
  module LRUMap = LRU.Make(K)
  type 'a t = 'a M.t LRUMap.cache

  type key = K.t
  type 'a deferred = 'a M.t
  type 'a validator = (K.t * 'a option) -> 'a M.t

  let create n = LRUMap.create n

  open! M
  let find cache key ~f =
    match LRUMap.find cache key with
    | None -> f (key,None)
    | Some old ->
        old >>? function
        | Ok data -> f (key, Some data)
        | Error _ -> f (key, None)

  let ret (_, v) = M.return v
  let get cache key = find cache key ~f:ret

  let set cache key value =
    LRUMap.replace cache key (M.return value)

  let with_validator cache (revalidate: 'a validator) key : 'a M.t =
    let pending = find cache key ~f:revalidate in
    (* ensure ordering: first add the deferred, then inspect result *)
    LRUMap.replace cache key pending;
    pending >>? function
    | Ok r -> M.return r
    | Error e -> M.fail e

  let with_cache cache f query =
    with_validator cache (function
      | key, None -> f key
      | _key, Some data -> M.return data
      ) query
end

module Direct = struct
  type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b
  type 'a t = ('a, exn) result
  let return x = Ok x
  let fail e = Error e
  let (>>?) v f = f v
end

module PendingLimit = Pendinglimit
include Make(String)(Direct)
