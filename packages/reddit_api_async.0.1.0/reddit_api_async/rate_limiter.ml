open! Core
open! Async

module type S = sig
  type t [@@deriving sexp_of]

  val kind : string

  val with_t
    :  t
    -> f:(unit -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
    -> time_source:Time_source.t
    -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
end

type t = T : (module S with type t = 't) * 't -> t

let sexp_of_t (T ((module S), t)) : Sexp.t = List [ Atom S.kind; [%sexp_of: S.t] t ]
let with_t (T ((module S), t)) = S.with_t t

module With_minimum_delay = struct
  type t =
    { ready : (unit, read_write) Mvar.t
    ; delay : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let kind = "With_minimum_delay"

  let create ~delay =
    let ready = Mvar.create () in
    Mvar.set ready ();
    { ready; delay }
  ;;

  let with_t { ready; delay } ~f ~time_source =
    let%bind () = Mvar.take ready in
    Deferred.upon (Time_source.after time_source delay) (fun () -> Mvar.set ready ());
    f ()
  ;;
end

module By_headers = struct
  let kind = "By_headers"

  module Server_side_info = struct
    type t =
      { remaining_api_calls : int
      ; reset_time : Time_ns.t
      ; server_time : Time_ns.t
      }
    [@@deriving sexp, fields]

    let snap_to_nearest_minute time =
      let interval = Time_ns.Span.minute in
      let base = Time_ns.epoch in
      let candidates =
        [ Time_ns.prev_multiple ~can_equal_before:true ~interval ~base ~before:time ()
        ; Time_ns.next_multiple ~can_equal_after:false ~interval ~base ~after:time ()
        ]
      in
      List.min_elt
        candidates
        ~compare:
          (Comparable.lift Time_ns.Span.compare ~f:(fun time' ->
               Time_ns.abs_diff time time'))
      |> Option.value_exn
    ;;

    let%expect_test _ =
      List.iter [ "2020-11-30 18:48:01.02Z"; "2020-11-30 18:47:59.02Z" ] ~f:(fun time ->
          let time = Time_ns.of_string time in
          print_s [%sexp (snap_to_nearest_minute time : Time_ns.Alternate_sexp.t)]);
      [%expect {|
          "2020-11-30 18:48:00Z"
          "2020-11-30 18:48:00Z" |}]
    ;;

    let parse_http_header_date date_string =
      Scanf.sscanf
        date_string
        "%3s, %2d %3s %4d %2d:%2d:%2d GMT"
        (fun day_of_week d month y hr min sec ->
          let day_of_week = Day_of_week.of_string day_of_week in
          let month = Month.of_string month in
          let date = Date.create_exn ~y ~m:month ~d in
          (match Day_of_week.equal day_of_week (Date.day_of_week date) with
          | true -> ()
          | false ->
            raise_s
              [%message
                "HTTP response: Day of week did not match parsed date"
                  (day_of_week : Day_of_week.t)
                  (date : Date.t)
                  (date_string : string)]);
          let ofday = Time_ns.Ofday.create ~hr ~min ~sec () in
          Time_ns.of_date_ofday date ofday ~zone:Time_ns.Zone.utc)
    ;;

    let%expect_test _ =
      print_s
        [%sexp
          (parse_http_header_date "Wed, 21 Oct 2015 07:28:00 GMT"
            : Time_ns.Alternate_sexp.t)];
      [%expect {| "2015-10-21 07:28:00Z" |}]
    ;;

    let t_of_headers headers =
      let get_header header =
        match Cohttp.Header.get headers header with
        | Some v -> v
        | None ->
          raise_s
            [%message
              "Missing expected X-Ratelimit header"
                (header : string)
                (headers : Cohttp.Header.t)]
      in
      let remaining_api_calls =
        get_header "X-Ratelimit-Remaining" |> Float.of_string |> Int.of_float
      in
      let server_time = parse_http_header_date (get_header "Date") in
      let reset_time =
        let relative_reset_time =
          get_header "X-Ratelimit-Reset" |> Int.of_string |> Time_ns.Span.of_int_sec
        in
        snap_to_nearest_minute (Time_ns.add server_time relative_reset_time)
      in
      { remaining_api_calls; reset_time; server_time }
    ;;

    let demonstrates_reset old_t new_t = Time_ns.( < ) old_t.reset_time new_t.reset_time

    let update t t' =
      match Time_ns.( <= ) t.server_time t'.server_time with
      | true -> t'
      | false -> t
    ;;
  end

  type t =
    { mutable server_side_info : Server_side_info.t option
    ; mutable waiting_for_reset : bool
    ; jobs : (unit -> unit) Queue.t
    }
  [@@deriving sexp_of]

  let create () =
    let short_timer_ready = Mvar.create () in
    Mvar.set short_timer_ready ();
    { server_side_info = None; waiting_for_reset = false; jobs = Queue.create () }
  ;;

  let rec clear_queue t ~time_source =
    match t.waiting_for_reset with
    | true -> ()
    | false ->
      Queue.dequeue t.jobs
      |> Option.iter ~f:(fun job ->
             match t.server_side_info with
             | None ->
               t.waiting_for_reset <- true;
               job ()
             | Some
                 ({ reset_time; remaining_api_calls; server_time = _ } as
                 server_side_info) ->
               (match remaining_api_calls with
               | 0 ->
                 t.waiting_for_reset <- true;
                 upon (Time_source.at time_source reset_time) job
               | n ->
                 t.server_side_info
                   <- Some { server_side_info with remaining_api_calls = n - 1 };
                 job ();
                 clear_queue t ~time_source))
  ;;

  let update_server_side_info t new_server_side_info =
    t.server_side_info
      <- Some
           (match t.server_side_info with
           | None -> new_server_side_info
           | Some server_side_info ->
             Server_side_info.update server_side_info new_server_side_info)
  ;;

  let with_t t ~f ~time_source =
    Deferred.create (fun ivar ->
        Queue.enqueue t.jobs (fun () ->
            upon (f ()) (fun ((response, _body) as result) ->
                let headers = Cohttp.Response.headers response in
                let new_server_side_info = Server_side_info.t_of_headers headers in
                (match t.server_side_info with
                | None -> t.waiting_for_reset <- false
                | Some old_server_side_info ->
                  (match
                     Server_side_info.demonstrates_reset
                       old_server_side_info
                       new_server_side_info
                   with
                  | false -> ()
                  | true -> t.waiting_for_reset <- false));
                update_server_side_info t new_server_side_info;
                Ivar.fill ivar result;
                clear_queue t ~time_source));
        clear_queue t ~time_source)
  ;;
end

let by_headers () = T ((module By_headers), By_headers.create ())

let with_minimum_delay ~delay =
  T ((module With_minimum_delay), With_minimum_delay.create ~delay)
;;
