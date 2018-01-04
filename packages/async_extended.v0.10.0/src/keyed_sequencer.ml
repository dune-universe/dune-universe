open Core
open Async

module Throttle_and_count : sig
  type t

  val create   : max_concurrent_jobs:int -> t
  val enqueue  : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

    (* see .mli for documentation *)
  val has_empty_spot : t -> bool

    (* [is_empty t] is true if all jobs that were ever enqueued in this
       [Throttle_and_count] have been completed. *)
  val is_empty : t -> bool
end = struct
  type t =
    { mutable count : int
    ; throttle : unit Throttle.t
    ; max_concurrent_jobs : int
    }

  let create ~max_concurrent_jobs =
    { count = 0
    ; throttle = Throttle.create ~max_concurrent_jobs ~continue_on_error:true
    ; max_concurrent_jobs
    }

  let enqueue t f =
    t.count <- t.count + 1;
    Throttle.enqueue' t.throttle f
    >>| fun x ->
    t.count <- t.count - 1;
    match x with
    | `Aborted -> assert false (* always set [continue_on_error] to true *)
    | `Raised exn -> raise exn
    | `Ok x -> x

  let has_empty_spot t = Int.(t.count < t.max_concurrent_jobs)
  let is_empty       t = Int.(=) 0 t.count
end

type 'a t =
  { shared_throttle : Throttle_and_count.t option
  ; sequencers      : ('a, Throttle_and_count.t) Hashtbl.t
  ; on_exn          : ('a -> exn -> unit) option
  }

let create ~hashable ?on_exn ?max_total_concurrent_jobs () =
  { shared_throttle =
      Option.map max_total_concurrent_jobs ~f:(fun max_concurrent_jobs ->
        Throttle_and_count.create ~max_concurrent_jobs)
  ; sequencers =  Hashtbl.Using_hashable.create ~hashable ()
  ; on_exn
  }

let enqueue t ~key f =
  let enqueue () =
    let sequencer =
      Hashtbl.find_or_add t.sequencers key ~default:(fun () ->
        Throttle_and_count.create ~max_concurrent_jobs:1)
    in
    Throttle_and_count.enqueue sequencer (fun () ->
      Monitor.try_with f
      >>= function
      | Error exn ->
        Option.iter t.on_exn ~f:(fun f -> f key exn);
        raise exn
      | Ok result -> return (result, sequencer))
  in
  begin match t.shared_throttle with
  | None          -> enqueue ()
  | Some throttle -> Throttle_and_count.enqueue throttle enqueue
  end
  >>= fun (result, sequencer) ->
  begin
    if Throttle_and_count.is_empty sequencer
    then Hashtbl.remove t.sequencers key
  end;
  return result

let has_empty_spot t ~key =
  let is_nonexistant_or_has_empty_spot = function
    | None -> true
    | Some sequencer -> Throttle_and_count.has_empty_spot sequencer
  in
  is_nonexistant_or_has_empty_spot (Hashtbl.find t.sequencers key)
  && is_nonexistant_or_has_empty_spot t.shared_throttle

let run_now t ~key f =
  if has_empty_spot t ~key
  then `running (enqueue t ~key f)
  else `no_empty_spots
