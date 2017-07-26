open Core
open Import

let debug = false

let sample_every = sec 1.

module Sampler : sig
  type t [@@deriving sexp_of]

  val create : unit -> t

  val take_sample : t -> Percent.t
end = struct
  type t =
    { mutable last_usage : Time.Span.t
    ; mutable last_time  : Time.t
    }
  [@@deriving sexp_of]

  let time_used () =
    let r = Core.Unix.Resource_usage.get `Self in
    sec (r.utime +. r.stime)
  ;;

  let take_sample t =
    let usage_now = time_used () in
    let time_now = Time.now () in
    let sample =
      Time.Span.((usage_now - t.last_usage) // Time.diff time_now t.last_time)
      |> Percent.of_mult
    in
    t.last_usage <- usage_now;
    t.last_time  <- time_now;
    sample
  ;;

  let create () =
    { last_usage = time_used ()
    ; last_time  = Time.now ()
    }
  ;;
end

(* The practical test of this module is in tools/cpu_usage_test *)
module Samples : sig
  type t

  val create : unit -> t

  val subscribe : t -> Percent.t Pipe.Reader.t
end = struct
  type t =
    { sampler     : Sampler.t
    ; subscribers : Percent.t Pipe.Writer.t Bag.t
    }
  [@@deriving sexp_of]

  let take_sample t =
    let sample = Sampler.take_sample t.sampler in
    Bag.iter t.subscribers ~f:(fun w ->
      Pipe.write_without_pushback_if_open w sample);
  ;;

  let subscribe t =
    let r, w = Pipe.create () in
    let token = Bag.add t.subscribers w in
    upon (Pipe.closed r) (fun () -> Bag.remove t.subscribers token);
    r
  ;;

  let create () =
    let t =
      { sampler     = Sampler.create ()
      ; subscribers = Bag.create ()
      }
    in
    every ~start:(Clock.after sample_every) sample_every (fun () -> take_sample t);
    t
  ;;
end

module Summary = struct
  type t =
    { min : Percent.t
    ; max : Percent.t
    ; avg : Percent.t
    }
  [@@deriving bin_io, sexp]
end

module Summaries : sig

  type t

  include Invariant.S with type t := t

  val create : Percent.t Pipe.Reader.t -> t

  val subscribe : t -> Time.Span.t list -> (Time.Span.t * Summary.t) Pipe.Reader.t

end = struct

  let max_num_samples_when_no_subscribers = 0

  module Subscriber = struct
    type t =
      { write_summaries_to : (Time.Span.t * Summary.t) Pipe.Writer.t
      ; window_duration    : Time.Span.t
      ; num_samples        : int
      }
    [@@deriving fields, sexp_of]

    let invariant t =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~write_summaries_to:(check Pipe.Writer.invariant)
          ~window_duration:ignore
          ~num_samples:(check (fun num_samples -> assert (num_samples > 0))))
    ;;

    let create ~write_summaries_to ~window_duration =
      let num_samples =
        Float.iround_up_exn (Time.Span.( // ) window_duration sample_every)
      in
      if num_samples <= 0
      then failwiths "invalid window duration" window_duration [%sexp_of: Time.Span.t];
      let window_duration = sec (Int.to_float num_samples) in
      { write_summaries_to; window_duration; num_samples }
    ;;

    let compare_num_samples t1 t2 = Int.compare t1.num_samples t2.num_samples

    let max_num_samples ts =
      List.fold ts ~init:max_num_samples_when_no_subscribers
        ~f:(fun max t -> Int.max max t.num_samples)
    ;;
  end

  type t =
    { mutable max_num_samples : int
    (* [samples] has the most recent samples, up to [max_num_samples], ordered by sample
       time, with the most recent sample at the front and the least recent at the back. *)
    ; samples                 : Percent.t Deque.t
    (* [subscribers] is kept sorted by increasing [num_samples] so that [update] can be
       more efficient. *)
    ; mutable subscribers     : Subscriber.t list
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~max_num_samples:(check (fun max_num_samples ->
          assert (max_num_samples = Subscriber.max_num_samples t.subscribers)))
        ~samples:(check (fun samples ->
          assert (Deque.length samples <= t.max_num_samples)))
        ~subscribers:(check (fun subscribers ->
          List.iter subscribers ~f:Subscriber.invariant;
          assert (List.is_sorted subscribers ~compare:Subscriber.compare_num_samples))))
  ;;

  let drop_old_samples_if_necessary t =
    let num_samples = Deque.length t.samples in
    if num_samples > t.max_num_samples
    then Deque.drop_back t.samples ~n:(num_samples - t.max_num_samples);
  ;;

  let add_sample t sample =
    if debug then invariant t;
    Deque.enqueue_front t.samples sample;
    drop_old_samples_if_necessary t;
    (* use local refs to keep the fold code cleaner *)
    let sum         = ref Percent.zero in
    let num_samples = ref 0            in
    let max         = ref (Percent.of_mult Float.min_value) in
    let min         = ref (Percent.of_mult Float.max_value) in
    let process_one processed_subscribers (subscriber : Subscriber.t) =
      if Pipe.is_closed subscriber.write_summaries_to
      then processed_subscribers
      else begin
        let avg =
          Percent.to_mult !sum /. Int.to_float !num_samples
          |> Percent.of_mult
        in
        let summary =
          (subscriber.window_duration,
           { Summary.
             avg
           ; max = !max
           ; min = !min
           })
        in
        Pipe.write_without_pushback subscriber.write_summaries_to summary;
        subscriber :: processed_subscribers;
      end
    in
    let (unprocessed_subscribers, rev_processed_subscribers) =
      Deque.fold' t.samples `front_to_back
        ~init:(t.subscribers, [])
        ~f:(fun ((subscribers, processed_subscribers) as res) sample ->
          match subscribers with
          | [] -> res
          | subscriber :: remaining_subscribers ->
            incr num_samples;
            sum := Percent.(+) !sum sample;
            max := Percent.max !max sample;
            min := Percent.min !min sample;
            if !num_samples < subscriber.num_samples
            then subscribers, processed_subscribers
            else begin
              assert (!num_samples = subscriber.num_samples);
              let processed_subscribers =
                process_one processed_subscribers subscriber
              in
              remaining_subscribers, processed_subscribers
            end)
    in
    (* Process remaining subscribers whose [num_samples] is larger than the number of
       samples we currently have. *)
    let rev_processed_subscribers =
      List.fold unprocessed_subscribers ~init:rev_processed_subscribers ~f:process_one
    in
    t.subscribers <- List.rev rev_processed_subscribers;
    t.max_num_samples <-
      (match rev_processed_subscribers with
       | [] -> max_num_samples_when_no_subscribers
       | subscriber :: _ -> subscriber.num_samples);
    drop_old_samples_if_necessary t;
  ;;

  let create samples =
    let t =
      { max_num_samples = max_num_samples_when_no_subscribers
      ; samples         = Deque.create ()
      ; subscribers     = []
      }
    in
    don't_wait_for (Pipe.iter_without_pushback samples
                      ~f:(fun sample -> add_sample t sample));
    t
  ;;

  let subscribe t windows =
    if List.is_empty windows
    then Pipe.of_list []
    else begin
      let read_summaries_from, write_summaries_to = Pipe.create () in
      let subscribers =
        List.map windows ~f:(fun window_duration ->
          Subscriber.create ~write_summaries_to ~window_duration)
        |> List.sort ~cmp:Subscriber.compare_num_samples
      in
      t.subscribers <-
        List.merge subscribers t.subscribers ~cmp:Subscriber.compare_num_samples;
      t.max_num_samples <- Subscriber.num_samples (List.last_exn t.subscribers);
      read_summaries_from
    end
  ;;

  let%test_module _ =
    (module struct
      (* Check correct sample sizes and removal of dead clients *)
      let%test_unit _ =
        let windows1 = [ sec 5. ; sec 8. ; sec 10. ] in
        let windows2 = [ sec 4. ; sec 3. ; sec 6.  ] in
        Async_unix.Thread_safe.block_on_async_exn (fun () ->
          let pr, pw = Pipe.create () in
          let t = create pr in
          let r2 = subscribe t windows2 in
          assert (t.max_num_samples = 6);
          let r1 = subscribe t windows1 in
          assert (t.max_num_samples = 10);
          Pipe.close_read r1;
          Pipe.write_without_pushback pw (Percent.of_mult 1.);
          Pipe.downstream_flushed pw
          >>= fun res ->
          assert (res = `Ok);
          assert (t.max_num_samples = 6);
          Pipe.close_read r2;
          Pipe.write_without_pushback pw (Percent.of_mult 2.);
          Pipe.downstream_flushed pw
          >>| fun res ->
          assert (res = `Ok);
          assert (t.max_num_samples = 0))

      (* Check that summary computation is correct *)
      let%test_unit _ =
        let robustly_test_eq pct1 pct2 =
          assert (0 = Float.robustly_compare (Percent.to_mult pct1) (Percent.to_mult pct2))
        in
        let window = [ sec 4.] in
        let index = [0  ; 1  ; 2  ; 3  ; 4  ; 5 ] in
        let pc = List.map ~f:Percent.of_percentage in
        let input = pc [4. ; 8. ; 3. ; 1. ; 0. ; 2. ] in
        let avg   = pc [4. ; 6. ; 5. ; 4. ; 3. ; 1.5 ] in
        let max   = pc [4. ; 8. ; 8. ; 8. ; 8. ; 3. ] in
        let min   = pc [4. ; 4. ; 3. ; 1. ; 0. ; 0. ] in
        Async_unix.Thread_safe.block_on_async_exn (fun () ->
          let pr, pw = Pipe.create () in
          let t = create pr in
          let r = subscribe t window in
          Deferred.List.iter index ~f:(fun i ->
            Pipe.write_without_pushback pw (List.nth_exn input i);
            Pipe.downstream_flushed pw
            >>= fun res ->
            assert (res = `Ok);
            Pipe.read r
            >>| function
            | `Ok (_, summary) ->
              robustly_test_eq summary.avg (List.nth_exn avg i);
              robustly_test_eq summary.max (List.nth_exn max i);
              robustly_test_eq summary.min (List.nth_exn min i);
            | _ -> assert false))
    end)
end

let samples =
  let t = lazy (Samples.create ()) in
  fun () -> Samples.subscribe (Lazy.force t)
;;

let summaries =
  let t = lazy (Summaries.create (samples ())) in
  fun ~windows ->
    Summaries.subscribe (Lazy.force t) windows
;;
