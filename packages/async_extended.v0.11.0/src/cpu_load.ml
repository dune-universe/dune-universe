open Core
open Async

module Span = Time.Span

module Cpu_time : sig
  type t

  val get : unit -> t
  val sub : t -> t -> t
  val to_span : t -> Time.Span.t
end = struct
  type t = Time.Span.t

  let get () =
    let tms = Unix.times () in
    sec (tms.Unix.tms_utime +. tms.Unix.tms_stime)
  ;;

  let sub = Span.(-)

  let to_span t = t
end

module Tick : sig
  type t

  val create : unit -> t
  val at : t -> Time.t
  val load : start:t -> finish:t -> float
end = struct
  type t = {
    at : Time.t;
    consumed : Cpu_time.t;
  }

  let at t = t.at

  let create () = {
    at = Time.now ();
    consumed = Cpu_time.get ();
  }

  let load ~start ~finish =
    Time.Span.to_sec (Cpu_time.to_span (Cpu_time.sub finish.consumed start.consumed))
      /. Time.Span.to_sec (Time.diff finish.at start.at)
  ;;
end

type t = {
  at_start : Tick.t;
  mutable most_recent : Tick.t;
  mutable a_second_ago : Tick.t;
  mutable a_minute_ago : Tick.t;
  within_a_minute : Tick.t Queue.t;
}

let r = ref None

let start () =
  let tick = Tick.create () in
  let t = {
    at_start = tick;
    most_recent = tick;
    a_second_ago = tick;
    a_minute_ago = tick;
    within_a_minute = Queue.create ();
  } in
  r := Some t;
  Clock.every (sec 1.) (fun () ->
    t.a_second_ago <- t.most_recent;
    let tick = Tick.create () in
    t.most_recent <- tick;
    Queue.enqueue t.within_a_minute tick;
    let a_minute_ago = Time.sub (Tick.at tick) (Time.Span.of_min 1.) in
    let rec loop () =
      match Queue.peek t.within_a_minute with
      | None -> ()
      | Some tick ->
          if Time.(<=) (Tick.at tick) a_minute_ago then begin
            ignore (Queue.dequeue_exn t.within_a_minute);
            t.a_minute_ago <- tick;
            loop ();
          end
    in
    loop ();
  );
;;

module Stats = struct
  type t = {
    last_second : float;
    last_minute : float;
    since_start : float;
  } [@@deriving sexp]

end

let get () =
  match !r with
  | None -> failwithf "called Cpu_load.get without calling Cpu_load.start" ()
  | Some t ->
      let load finish = Tick.load ~start:t.at_start ~finish in
      { Stats.
        last_second = load t.a_second_ago;
        last_minute = load t.a_minute_ago;
        since_start = load t.most_recent;
      }
;;


