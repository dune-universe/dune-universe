open Core
open Async

module Cached_data = struct
  type 'a t =
    { mutable last_read : Time.t
    ; data : 'a Or_error.t
    }

  let last_read_age t ~now = Time.diff now t.last_read

  let read t ~now =
    t.last_read <- now;
    t.data

  let create data ~now = {last_read = now; data}
end

type ('a, 'b) t =
  { cache : ('a, 'b Cached_data.t) Hashtbl.t
  ; sequencer : 'a Keyed_sequencer.t
  ; cache_errors : bool
  ; add_to_cache : ('a -> 'b Or_error.t Deferred.t)
  ; to_remove : ('b -> unit Deferred.t)
  ; remove_if_unread_for : Time.Span.t option
  ; max_cached_data_age : Time.Span.t option
  }

let create ~add_to_cache ~to_remove ~hashable ?on_exn ?(cache_errors=false)
    ?remove_if_unread_for ?max_cached_data_age ?max_total_concurrent_jobs () =
  { cache = Hashtbl.Using_hashable.create ~hashable ()
  ; sequencer = Keyed_sequencer.create ~hashable ?on_exn ?max_total_concurrent_jobs ()
  ; cache_errors
  ; add_to_cache
  ; to_remove
  ; remove_if_unread_for
  ; max_cached_data_age
  }

let change t ~key ~f =
  Keyed_sequencer.enqueue t.sequencer ~key (fun () ->
    let data = Hashtbl.find t.cache key in
    f data
    >>= fun data ->
    Hashtbl.change t.cache key ~f:(fun _ -> data);
    return ())

let remove_data t cached =
  match cached.Cached_data.data with
  | Error _err -> return None
  | Ok data -> t.to_remove data >>| fun () -> None

let consider_removing t key =
  match t.remove_if_unread_for with
  | None -> ()
  | Some remove_if_unread_for ->
    let rec consider_removing_forever t key =
      don't_wait_for (change t ~key ~f:(function
      | None -> return None
      | Some data ->
        let now = Time.now () in
        if Time.Span.(>=) (Cached_data.last_read_age data ~now) remove_if_unread_for
        then remove_data t data
        else begin
          Clock.at (Time.add data.Cached_data.last_read remove_if_unread_for)
          >>> (fun () -> consider_removing_forever t key);
          return (Some data)
        end))
    in
    Clock.after remove_if_unread_for >>> fun () -> consider_removing_forever t key

let remove t ~key =
  change t ~key ~f:(function
  | None -> return None
  | Some cached_data -> remove_data t cached_data)

  (* We want to enter the sequencer as infrequently as possible. But, once inside the
     sequencer, make sure that jobs queued ahead of the current job didn't make a further
     add_to_cache call redundant. This is why we call find in the hashtable twice. *)
let find t ~key =
  let now = Time.now () in
  let cache_and_return data =
    let data = Cached_data.create ~now data in
    Hashtbl.set t.cache ~key ~data;
    consider_removing t key;
    Option.iter t.max_cached_data_age ~f:(fun age ->
      Clock.after age >>> fun () -> don't_wait_for (remove t ~key));
    Cached_data.read ~now data
  in
  let try_to_add t ~key =
    t.add_to_cache key
    >>| fun data ->
    if Result.is_ok data || t.cache_errors
    then cache_and_return data
    else data
  in
  Keyed_sequencer.enqueue t.sequencer ~key (fun () ->
    match Hashtbl.find t.cache key with
    | Some data -> return (Cached_data.read ~now data)
    | None -> try_to_add t ~key)

let find_cached_only t ~key =
  Option.bind (Hashtbl.find t.cache key) ~f:(fun data ->
    Result.ok (Cached_data.read data ~now:(Time.now ())))
