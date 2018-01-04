open Core
open Async

type 'a t = {
  heap: 'a Heap.t;
  (* if [pop_waiters] is not empty, then [heap] must be empty, we could use a variant
     here, but we dont to avoid creating heap and queue repeatly *)
  pop_waiters: 'a Ivar.t Queue.t
}

let length v = Heap.length v.heap

let create ~cmp =
  { heap = Heap.create ~cmp ();
    pop_waiters = Queue.create ()
  }

let push t v =
  match Queue.dequeue t.pop_waiters with
  | Some ivar -> Ivar.fill ivar v
  | None      -> Heap.add t.heap v

let pop t =
  match Heap.pop t.heap with
  | Some v -> return v
  | None -> Deferred.create (fun ivar -> Queue.enqueue t.pop_waiters ivar)

let iter_and_clear t ~f =
  if not (Heap.is_empty t.heap) then (
    let rec loop () =
      match Heap.pop t.heap with
      | None -> ()
      | Some v -> f v; loop ()
    in
    loop ()
  )

let clear t = iter_and_clear t ~f:ignore
