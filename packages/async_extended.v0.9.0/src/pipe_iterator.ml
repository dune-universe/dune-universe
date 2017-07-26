open Core
open Async


(* Shared data between all iterators *)
module Shared = struct
  type 'a t = {
    get_next: (unit -> [`Ok of 'a | `Eof] Deferred.t);
    close_data: (unit -> unit);
    close_iterators: (unit -> unit) Bag.t;
    mutable data_closed: bool;
  }

  let close_data t =
    if not t.data_closed
    then begin
      t.data_closed <- true;
      t.close_data ();
    end

  let close_everything t =
    Bag.until_empty t.close_iterators (fun close_iterator -> close_iterator ());
    close_data t

  let next t =
    match%map t.get_next () with
    | `Eof -> close_data t; `Eof
    | `Ok _ as x -> x

end

(* A singly-linked list that is shared between all iterator copies. This allows iterator
   copies to be at different points in the sequence without loading the sequence more than
   once, and the fact that pointers only go forward means that the head of the sequence is
   automatically garbage-collectable after all iterators advance past it. *)
type 'a node = {
  mutable next: 'a next
}
and 'a next =
  | Next of 'a * 'a node
  | Waiting of unit Deferred.t
  | Not_computed
  | Eof

(* An iterator *)
type 'a t = {
  mutable node: 'a node;
  shared: 'a Shared.t;
  mutable closed: bool;
  close_iterators_elt: (unit -> unit) Bag.Elt.t;
}

let close_iterator t =
  if not t.closed
  then begin
    t.closed <- true;
    t.node <- { next = Eof; };
    if Bag.mem_elt t.shared.close_iterators t.close_iterators_elt
    then Bag.remove t.shared.close_iterators t.close_iterators_elt;

    if Bag.is_empty t.shared.close_iterators
    then Shared.close_data t.shared;
  end

let create ~f ~close =
  let shared = {
    Shared.
    get_next = f;
    close_data = close;
    close_iterators = Bag.create ();
    data_closed = false;
  }
  in
  (* Work around restrictions on let rec by stuffing into a buffer *)
  let tbuf = ref None in
  let t = {
    node = { next = Not_computed; };
    shared = shared;
    closed = false;
    close_iterators_elt =
      Bag.add shared.close_iterators (fun () -> close_iterator (Option.value_exn !tbuf))
  }
  in
  tbuf := Some t;
  t

let copy t =
  if t.closed
  then failwith "[copy] called on closed Pipe_iterator"
  else {
    node = t.node;
    shared = t.shared;
    closed = false;
    close_iterators_elt = Bag.add t.shared.close_iterators (fun () -> close_iterator t)
  }

let extend_sequence shared node =
  let wait =
    match%map Shared.next shared with
    | `Eof -> node.next <- Eof
    | `Ok x -> node.next <- Next (x, { next = Not_computed })
  in
  node.next <- Waiting wait;
  wait

let rec read t =
  if t.closed
  then failwith "[read] called on closed Pipe_iterator or was ongoing when close occured";
  match t.node.next with
  | Eof -> return `Eof
  | Next (x,next) -> t.node <- next; return (`Ok x)
  | Waiting wait -> wait >>= fun () -> read t
  | Not_computed -> extend_sequence t.shared t.node >>= fun () -> read t

let rec peek t =
  if t.closed
  then failwith "[peek] called on closed Pipe_iterator or was ongoing when close occured";
  match t.node.next with
  | Eof -> return `Eof
  | Next (x,_next) -> return (`Ok x)
  | Waiting wait -> wait >>= fun () -> peek t
  | Not_computed -> extend_sequence t.shared t.node >>= fun () -> peek t

let read_now t =
  if t.closed
  then failwith "[read_now] called on closed Pipe_iterator";
  match t.node.next with
  | Eof -> `Eof
  | Next (x,next) -> t.node <- next; (`Ok x)
  | Waiting _
  | Not_computed
    -> `Nothing_available

let peek_now t =
  if t.closed
  then failwith "[peek_now] called on closed Pipe_iterator";
  match t.node.next with
  | Eof -> `Eof
  | Next (x,_next) -> (`Ok x)
  | Waiting _
  | Not_computed
    -> `Nothing_available


let of_pipe pipe =
  create ~f:(fun () -> Pipe.read pipe) ~close:(fun () -> Pipe.close_read pipe)

let with_pipe pipe ~f =
  let t = of_pipe pipe in
  Monitor.protect
    ~finally:(fun () -> Shared.close_everything t.shared; Deferred.unit)
    (fun () -> f t)

let close = close_iterator
let close_everything t = Shared.close_everything t.shared
