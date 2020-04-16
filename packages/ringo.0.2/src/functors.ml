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

(* Main functor that is exported to the outside *)
module Make
  (Collection : Sigs.COLLECTION)
  (Tabler: Sigs.TABLER)
  (H: Hashtbl.HashedType)
  : Sigs.CACHE with type key = H.t
  = struct

  module Table = Tabler(H)

  type key = H.t

  type 'a t = {
     table : (key * 'a) Collection.node Table.t;
     collection : (key * 'a) Collection.t
  }

  let create n = {table = Table.create n; collection = Collection.create n}

  let add {collection; table} k v =
    begin match Collection.add_and_return_erased collection (k, v) with
      | node, Some (kerased, _verased) ->
            Table.remove table kerased;
            Table.replace table k node
      | node, None ->
            Table.replace table k node
    end

  let find_opt {table; collection} k =
    match Table.find_opt table k with
    | None -> None
    | Some node ->
          Collection.promote collection node;
        let (_, v) = Collection.data node in
        Some v

  let fold f {collection; _} acc =
    Collection.fold
      collection
      ~init:acc
      ~f:(fun acc kv ->
            let (k, v) = Collection.data kv in
            f k v acc)

  let iter f t = fold (fun k v () -> f k v) t ()

  let remove {table; collection} k =
     (* NOTE: in some collections, [remove] is a noop which leads to sloppy
        counting *)
    match Table.find_opt table k with
    | None -> ()
    | Some node ->
       Collection.remove collection node;
       Table.remove table k

  let length {table; _} = Table.length table

  end


(* Functor to fake the boxedness of collections. *)
module Box (C: Sigs.UNBOXED_COLLECTION) : Sigs.COLLECTION = struct

   include C

   type 'a node = 'a
   let data x = x

   let add r x = add r x; x
   let add_and_return_erased r x =
     let erased = add_and_return_erased r x in
     (x, erased)
   let add_list r xs =
     add_list r xs;
     Utils.n_last xs (capacity r)

   let remove _ _ = ()
   let promote _ _ = ()

   let elements_data = elements

end

module Unbox (C: Sigs.COLLECTION) : Sigs.UNBOXED_COLLECTION = struct

   include C

   let add dll x = ignore @@ add dll x
   let add_and_return_erased dll x =
      let (_, y) = add_and_return_erased dll x in
      y
   let add_list dll xs = ignore @@ add_list dll xs
   let fold dll ~init ~f =
      fold dll ~init ~f:(fun acc node -> f acc (data node))
   let elements dll =
      elements_data dll

end

module Promote_is_noop (C: Sigs.COLLECTION) : Sigs.COLLECTION = struct
   include C
   let promote _ _ = ()
end

module LRU_Collection = Dll
module FIFO_Sloppy_Collection = Box(Ring)
module FIFO_Precise_Collection = Promote_is_noop (Dll)
module Strict_tabler = Hashtbl.Make
module Loose_tabler = LooseTabler.Make
