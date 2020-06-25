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
module Make_map
  (Collection: Sigs.COLLECTION)
  (Tabler: Sigs.TABLER)
  (H: Hashtbl.HashedType)
: Sigs.CACHE_MAP with type key = H.t
= struct

  module H = H

  module Table = Tabler(H)

  type key = H.t

  type 'a t = {
     table: (key * 'a) Collection.node Table.t;
     collection: (key * 'a) Collection.t;
  }

  let create n = {table = Table.create n; collection = Collection.create n}

  let replace {collection; table} k v =
    begin match Table.find_opt table k with
       | Some node ->
           (* NOTE: in some collections, [remove] is a noop leading to sloppy counting *)
           Collection.remove collection node;
       | None -> ()
    end ;
    match Collection.add_and_return_erased collection (k, v) with
    | node, Some (kerased, _verased) ->
        Table.remove table kerased;
        Table.replace table k node
    | node, None ->
        Table.replace table k node

  let find_opt {table; collection} k =
    match Table.find_opt table k with
    | None -> None
    | Some node ->
        Collection.promote collection node;
        let (_, v) = Collection.data node in
        Some v

  let fold f {collection; _} init =
    let f acc kv =
      let (k, v) = Collection.data kv in
      f k v acc in
    Collection.fold collection ~init ~f

  let fold_v (f : 'a -> 'b -> 'b) {table; _} init =
    Table.fold_v (fun n acc ->
       let (_, v) = Collection.data n in
       f v acc)
    table init


  let remove {table; collection} k =
    match Table.find_opt table k with
    | None -> ()
    | Some node ->
       (* NOTE: in some collections, [remove] is a noop leading to sloppy counting *)
       Collection.remove collection node;
       Table.remove table k

  let length {table; _} = Table.length table

  let capacity {collection; _} = Collection.capacity collection

  let clear {table; collection} =
    Table.clear table;
    Collection.clear collection

end


module Make_set
  (Collection: Sigs.COLLECTION)
  (Tabler: Sigs.TABLER)
  (H: Hashtbl.HashedType)
: Sigs.CACHE_SET with type elt = H.t
= struct

  module Table = Tabler(H)

  type elt = H.t

  type t = {
     table: elt Collection.node Table.t;
     collection: elt Collection.t
  }

  let create n = {table = Table.create n; collection = Collection.create n}

  let add {collection; table} v =
    match Table.find_opt table v with
    | Some node ->
        Collection.promote_write collection node
    | None ->
        match Collection.add_and_return_erased collection v with
        | node, Some erased ->
            Table.remove table erased;
            Table.replace table v node
        | node, None ->
            Table.replace table v node

  let mem {table; collection} v =
    match Table.find_opt table v with
    | None -> false
    | Some node ->
        Collection.promote_read collection node;
        true

  let fold f {collection; _} init =
    let f acc v = f (Collection.data v) acc in
    Collection.fold collection ~init ~f

  let remove {table; collection} v =
    match Table.find_opt table v with
    | None -> ()
    | Some node ->
        Collection.remove collection node;
        Table.remove table v

  let length {table; _} = Table.length table

  let capacity {collection; _} = Collection.capacity collection

  let clear {table; collection} =
    Table.clear table;
    Collection.clear collection

end


module Unbox (C: Sigs.COLLECTION_BARE) : Sigs.UNBOXED_COLLECTION = struct

   include C

   let add dll x = ignore @@ add dll x
   let add_and_return_erased dll x = snd @@ add_and_return_erased dll x
   let add_list dll xs = ignore @@ add_list dll xs
   let fold dll ~init ~f = fold dll ~init ~f:(fun acc node -> f acc (data node))
   let elements dll = elements_data dll

end

module LRU_Collection : Sigs.COLLECTION = struct
   include Dll
   let promote_read = promote
   let promote_write = promote
end
module FIFO_Sloppy_Collection : Sigs.COLLECTION = struct
   include Ring
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
   let promote_read _ _ = ()
   let promote_write c e = ignore @@ add c (data e)

   let elements_data = elements
end
module FIFO_Precise_Collection : Sigs.COLLECTION = struct
   include Dll
   let promote_read _ _ = ()
   let promote_write = promote
   let promote _ _ = ()
end

module Strong_tabler : Sigs.TABLER = Strong_tabler.Make
module Weak_tabler : Sigs.TABLER = Weak_tabler.Make
