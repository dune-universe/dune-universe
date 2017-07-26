open Core
open Async

type t = {
  mutable is_locked : bool;
  waiting : unit Ivar.t Queue.t;
}

let create () =
  { is_locked = false;
    waiting = Queue.create (); }
;;

let try_lock t =
  if t.is_locked then
    `Not_acquired
  else begin
    t.is_locked <- true;
    `Acquired
  end
;;

let lock t =
  match try_lock t with
  | `Not_acquired -> Deferred.create (fun ivar -> Queue.enqueue t.waiting ivar)
  | `Acquired -> Deferred.unit
;;

let unlock t =
  if t.is_locked then
    match Queue.dequeue t.waiting with
    | None -> t.is_locked <- false
    | Some ivar -> Ivar.fill ivar ()
;;

let resource t =
  Resource.create
    ~acquire:(fun () -> Deferred.map ~f:(fun x -> Ok x) (lock t))
    ~release:(fun () -> unlock t; return ())
