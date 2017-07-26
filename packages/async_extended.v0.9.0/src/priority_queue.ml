open Core
open Async

(* this type is the result of a very particular ordering desire.  All high priority
   items should come off the queue before all low priority items, but items should be
   dequeued in order of enqueing within a given priority. *)
type ('a,'b) t = {
  mutable id: int;
  heap: ('a * ('b * int)) Async_heap.t;
}

let length v = Async_heap.length v.heap

let create cmp =
  let cmp (_,a) (_,b) = Tuple2.compare ~cmp1:cmp ~cmp2:Int.compare a b in
  { id = 0;
    heap = Async_heap.create ~cmp
  }

let enqueue t ~priority v =
  Async_heap.push t.heap (v, (priority,t.id));
  t.id <- t.id + 1

let dequeue t = Async_heap.pop t.heap >>| fst

let clear t = Async_heap.clear t.heap

let iter_and_clear t ~f = Async_heap.iter_and_clear t.heap ~f:(fun (v, _pri) -> f v)
