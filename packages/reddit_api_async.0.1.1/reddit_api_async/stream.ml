open! Core
open! Async
open Reddit_api_kernel

module Backoff : sig
  type t

  val initial : t
  val increment : t -> t
  val after : t -> unit Deferred.t
end = struct
  type t = Time_ns.Span.t

  let initial = Time_ns.Span.second
  let max_val = Time_ns.Span.of_int_sec 16
  let increment t = Time_ns.Span.min max_val (Time_ns.Span.scale t 2.)
  let after = Clock_ns.after
end

open Continue_or_stop

let fold_helper l ~init ~f =
  let rec loop l ~state ~f =
    match l with
    | [] -> return (Continue state)
    | hd :: tl ->
      (match%bind f state hd with
      | Stop final -> return (Stop final)
      | Continue state -> loop tl ~state ~f)
  in
  loop l ~state:init ~f
;;

let fold_until_finished
    (type id)
    (module Id : Hashable.S with type t = id)
    connection
    ~get_listing
    ~get_before_parameter
    ~init
    ~f
    ~on_error
  =
  let module Bounded_set = Bounded_set.Make (Id) in
  let seen = Bounded_set.create ~capacity:300 in
  let rec loop ?(first_pass = false) state before backoff cache_busting_counter =
    let limit, cache_busting_counter =
      match before with
      | Some _ -> 100, cache_busting_counter
      | None -> 100 - cache_busting_counter, (cache_busting_counter + 1) mod 30
    in
    match%bind Connection.call connection (get_listing ~before ~limit) with
    | Error response ->
      (match%bind on_error state response with
      | Stop result -> return result
      | Continue state ->
        let backoff = Backoff.increment backoff in
        let%bind () = Backoff.after backoff in
        loop state before backoff cache_busting_counter)
    | Ok list ->
      let list =
        List.filter list ~f:(fun child ->
            not (Bounded_set.mem seen (get_before_parameter child : id)))
      in
      let most_recent_element = List.hd list in
      let list = List.rev list in
      List.iter list ~f:(fun child -> Bounded_set.add seen (get_before_parameter child));
      let continue state =
        let before = Option.map most_recent_element ~f:get_before_parameter in
        let backoff =
          match List.is_empty list with
          | true -> Backoff.increment backoff
          | false -> Backoff.initial
        in
        let%bind () = Backoff.after backoff in
        loop state before backoff cache_busting_counter
      in
      (match first_pass with
      | true -> continue state
      | false ->
        (match%bind fold_helper list ~init:state ~f with
        | Stop result -> return result
        | Continue state -> continue state))
  in
  loop ~first_pass:true init None Backoff.initial 0
;;

let fold
    (type id)
    (module Id : Hashable.S with type t = id)
    connection
    ~get_listing
    ~get_before_parameter
    ~init
    ~f
    ~on_error
  =
  let continue_after state x ~f =
    let%bind state = f state x in
    return (Continue state)
  in
  fold_until_finished
    (module Id)
    connection
    ~get_listing
    ~get_before_parameter
    ~init
    ~f:(continue_after ~f)
    ~on_error:(continue_after ~f:on_error)
;;

let iter
    (type id)
    (module Id : Hashable.S with type t = id)
    connection
    ~get_listing
    ~get_before_parameter
    ~f
  =
  fold
    (module Id)
    connection
    ~get_listing
    ~get_before_parameter
    ~init:()
    ~f:(fun () child -> f child)
    ~on_error:(fun () error ->
      Log.Global.error_s [%message "Received error response" (error : Api.Api_error.t)];
      return ())
;;
