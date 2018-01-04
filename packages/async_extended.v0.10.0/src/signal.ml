open Core
open Async

type 'a t = {
  eq : ('a -> 'a -> bool);
  sc : 'a Synccast.t;
  mutable value : 'a;
}

let create ?eq v = {
  eq = (match eq with None -> (fun a b -> a = b) | Some f -> f);
  sc = Synccast.create ();
  value = v;
}

let close t = Synccast.close t.sc

let get t = t.value

let set t v =
  let doit () =
    t.value <- v;
    Synccast.send t.sc v
  in
  if not (t.eq v t.value) then doit ()
;;

let send t v = set t v

let upon t ~f = Synccast.upon t.sc ~f

let next t = Synccast.next t.sc

let register t = Synccast.register t.sc

let register_init t ~init ~f = Synccast.register_init t.sc ~i:init ~f

let register_self_init t ~f =
  let init = (fun () -> f (get t) >>| (fun _ -> ())) in
  register_init t ~init ~f

let on_update t ?stop_condition f =
  let f' =
    match stop_condition with
    | None -> (fun a ->
      f a;
      Deferred.return `Continue)
    | Some s -> (fun a ->
      if s a then Deferred.return `Leave
      else
        begin
          f a;
          Deferred.return `Continue
        end)
  in
  Synccast.register t.sc ~f:f'

let create_repeater ?randomize ?stop_condition span =
  let repeater = create (Time.now ()) in
  let send_next_beat () =
    let span =
      match randomize with
      | None -> span
      | Some percent -> Time.Span.randomize ~percent:(Percent.of_mult percent) span
    in
    Deferred.upon (Clock.after span) (fun () ->
      send repeater (Scheduler.cycle_start ());
    );
    Deferred.unit
  in
  let f' =
    match stop_condition with
    | None -> (fun _ ->
        don't_wait_for (send_next_beat ());
        Deferred.return `Continue)
    | Some f -> (fun now ->
      if f now then
        begin
          close repeater;
          Deferred.return `Leave
        end
      else
        begin
          don't_wait_for (send_next_beat ());
          Deferred.return `Continue
        end)
  in
  register_init repeater ~init:(send_next_beat) ~f:f';
  repeater
