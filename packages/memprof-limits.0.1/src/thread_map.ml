module IMap = Map.Make (
  struct
    type t = int
    let compare = Stdlib.compare
  end)

type 'a map = { mutex : Mutex.t ; mutable map : 'a IMap.t }

let get_thread map th =
  (* Concurrent threads do not change the value for this thread. *)
  IMap.find_opt th map.map

(* For set_thread/remove_thread we need a lock *)

let with_lock map f x =
  Fun.with_resource
    ~acquire:(fun () -> Mutex.lock map.mutex) ()
    (fun () -> f x)
    ~release:(fun () -> Mutex.unlock map.mutex)

let set_thread map v =
  with_lock map @@ fun thread -> map.map <- IMap.add thread v map.map

let remove_thread map =
  with_lock map @@ fun thread -> map.map <- IMap.remove thread map.map

(* Public functions *)

(* ref is used to avoid race between reset and with_value *)
type 'a t = 'a map ref

let empty () = { mutex = Mutex.create () ; map = IMap.empty }

let make () = ref (empty ())

let reset tmap = tmap := empty ()

let current_thread () = Thread.id (Thread.self ())

let get tmap = get_thread !tmap (current_thread ())

let with_value tmap ~value f arg =
  let th = current_thread () in
  (* if the map is reset between acquire and release, make sure that
     release does not restore the old value in the wrong map. *)
  let map = !tmap in
  let acquire () = set_thread map value th in
  let release = match get_thread map th with
    | None -> (fun () -> remove_thread map th)
    | Some old_value -> (fun () -> set_thread map old_value th)
  in
  (* FIXME: needs masking here as there is a race between release
     and asynchronous exceptions *)
  Fun.with_resource
    ~acquire ()
    (fun () -> f arg)
    ~release
