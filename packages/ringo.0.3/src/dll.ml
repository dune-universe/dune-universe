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

type 'a node = {
   data: 'a;
   mutable prev: 'a node option;
   mutable next: 'a node option;
}

let data {data; _} = data

type 'a tt =
  | Empty of {capacity: int}
  | Inited of {
      capacity: int;
      mutable size: int;
      mutable first: 'a node;
      mutable last: 'a node;
   }

type 'a t = 'a tt ref

let create capacity =
  if capacity <= 0 then
    raise (Invalid_argument "Ringo.Dll.create: negative or null capacity")
  else
    ref (Empty {capacity})

let capacity dll =
  match !dll with
  | Empty {capacity}
  | Inited {capacity; _} -> capacity

let add_and_return_erased dll data =
  match !dll with
  | Empty {capacity} ->
      let node = {data; prev = None; next = None} in
      dll := Inited {capacity; size = 1; first = node; last = node;};
      (node, None)
  | Inited ({capacity = 1; size; first; last;} as dll) ->
      assert (size = 1);
      assert (first == last);
      assert (first.next = None);
      assert (first.prev = None);
      assert (last.next = None);
      assert (last.prev = None);
      let pops = last in
      let node = {data; prev = None; next = None} in
      dll.first <- node;
      dll.last <- node;
      (node, Some pops.data)
  | Inited ({capacity; size; first; last;} as dll) ->
      assert (first.prev = None);
      assert (last.next = None);
      if size < capacity then begin
        let node = {data; prev = None; next = Some first} in
        first.prev <- Some node;
        dll.first <- node;
        dll.size <- succ dll.size;
        (node, None)
      end else begin
        let pops = last in
        ( match last.prev with
        | Some new_last ->
            dll.last <- new_last;
            new_last.next <- None
        | None ->
            (* This requires
               (1) to have a single element,
               (2) to have reached capacity, and
               (3) to have a capacity > 1 *)
              assert false );
        pops.prev <- None;
        pops.next <- None;
        let node = {data; prev = None; next = Some dll.first} in
        first.prev <- Some node;
        dll.first <- node;
        (node, Some pops.data)
      end

let add dll data = fst @@ add_and_return_erased dll data

let add_list dll l =
  let capacity = capacity dll in
  let length = List.length l in
  if length < capacity then begin
    List.map (add dll) l |> List.rev
  end else begin
    List.fold_left
      (fun (index, acc) x ->
        if index < length - capacity then
          (index + 1, acc)
        else
          (index + 1, add dll x:: acc))
      (0, [])
      l
    |> fun (_, acc) -> List.rev acc
  end

let clear dll =
  match !dll with
  | Empty _ -> ()
  | Inited {capacity; _} -> dll := Empty {capacity}

let rec fold_node f acc node =
  let acc = f acc node in
  match node.next with
  | None -> acc
  | Some next -> fold_node f acc next

let fold dll ~init ~f =
  match !dll with
  | Empty _ -> init
  | Inited {first; _} -> fold_node f init first

let elements t = fold t ~init:[] ~f:(fun acc elt -> elt:: acc)

let elements_data t = fold t ~init:[] ~f:(fun acc elt -> elt.data:: acc)

let remove dll node =
  match !dll with
  | Empty _ -> assert false
  | Inited dll ->
      begin match (node.prev, node.next) with
      | (None, None) -> assert false
      | (None, Some next) ->
          next.prev <- None;
          dll.first <- next;
      | (Some prev, None) ->
          prev.next <- None;
          dll.last <- prev
      | (Some prev, Some next) ->
          prev.next <- node.next;
          next.prev <- node.prev 
      end;
      node.prev <- None;
      node.next <- None;
      dll.size <- pred dll.size

let promote dll node =
  match !dll with
  | Empty _ -> assert false
  | Inited dll ->
      if dll.first == node then
        ()
      else begin
        let prev_first = dll.first in
        (* first, promote neighbors *)
        begin match (node.prev, node.next) with
        | (None, None) -> assert false
        | (None, Some _next) -> assert false
        | (Some prev, None) ->
            prev.next <- None;
            dll.last <- prev
        | (Some prev, Some next) ->
            prev.next <- node.next;
            next.prev <- node.prev
        end;
        (* promote node to first *)
        prev_first.prev <- Some node;
        node.prev <- None;
        node.next <- Some dll.first;
        dll.first <- node
     end

