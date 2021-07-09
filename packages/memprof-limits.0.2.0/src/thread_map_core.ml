open Resource_bind

module IMap = Map.Make (
  struct
    type t = int
    let compare = Stdlib.compare
  end)

type 'a t = { mutex : Mutex.t ; mutable map : 'a IMap.t }

let create () = { mutex = Mutex.create () ; map = IMap.empty }

let current_thread () = Thread.id (Thread.self ())

let get s =
  (* Concurrent threads do not alter the value for the current
     thread, so we do not need a lock. *)
  IMap.find_opt (current_thread ()) s.map

(* For set and clear we need a lock *)

let set s v =
  let& () = Mutex_aux.with_lock s.mutex in
  let new_map = match v with
    | None -> IMap.remove (current_thread ()) s.map
    | Some v -> IMap.add (current_thread ()) v s.map
  in
  s.map <- new_map

let clear s =
  let& () = Mutex_aux.with_lock s.mutex in
  s.map <- IMap.empty
