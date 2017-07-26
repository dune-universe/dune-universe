open Core
open Core_profiler
open Core_profiler_disabled

type -'rw t =
  { (* This table maps raw interests (from the event generator) to
       (the filter we want to check, the original interest). *)
    interests : (Probe_id.t Interest.t list) Interest.Raw.I.Table.t
  ; id_map : Reader.Header.t
  }

let create (id_map : Reader.Header.t) =
  { interests = Interest.Raw.I.Table.create ()
  ; id_map
  }

let add_interest t interest =
  (* Check that value/delta filters are only applied to probes,
     and that the units on all filters are correct. *)
  begin
    match interest with
    | Interest.All _ -> ()
    | Interest.In_interval (_, Time_delta, iv_units, _) ->
      if iv_units <> Profiler_units.Nanoseconds
      then failwith "Units of time delta interval should be nanoseconds"
    | Interest.In_interval (_, (Value | Delta), iv_units, _) ->
      let units =
        Interest.spec interest t.id_map
        |> Probe_type.units
        |> Option.value_exn ~message:"Can't test the value / delta of a timer"
      in
      if units <> iv_units
      then failwith "Units of interval do not match units of Probe"
  end;

  (* Check that delta / time_delta filters are only applied to paths *)
  begin
    match interest with
    | Interest.All _ -> ()
    | Interest.In_interval (Group_path _,               (Delta | Time_delta), _, _) -> ()
    | Interest.In_interval (_, Value, _, _) -> ()

    | Interest.In_interval ((Group_point _ | Single _), (Delta | Time_delta), _, _) ->
      failwith "Can only filter time deltas / deltas when considering a path"
  end;

  Hashtbl.add_multi t.interests ~key:(Interest.raw interest) ~data:interest
;;

let read_only (t : _ t) = (t :> read t)

(* [add_interest] asserts that we only filter on legal subjects, so
   we can safely pass junk values in delta and time_delta when appropriate,
   and it won't match a case that uses them *)
let test_one ~value ~time_delta ~delta interest =
  match (interest : 'a Interest.t) with
  | All _ -> true
  | In_interval (_, sub, _, interval) ->
    let sub' =
      match sub with
      | Value -> value
      | Time_delta -> Time_ns.Span.to_int_ns time_delta
      | Delta -> delta
    in
    Interval.Int.contains interval sub'

let test t event =
  match (event : Event_generator.event) with
  | Timer (interest, _t) ->
    Hashtbl.find t.interests interest
    (* Since the only interests that can be registered for Timers are [All]... *)
    |> Option.value ~default:[]

  | Probe (interest, _t, value) ->
    Hashtbl.find t.interests interest
    |> Option.value ~default:[]
    |> List.filter ~f:(test_one ~value ~time_delta:Time_ns.Span.zero ~delta:0)

  | Timer_path { interest; time_delta; time=_ } ->
    Hashtbl.find t.interests interest
    |> Option.value ~default:[]
    |> List.filter ~f:(test_one ~value:0 ~time_delta ~delta:0)

  | Probe_path { interest; time_delta; delta; value; time=_ } ->
    Hashtbl.find t.interests interest
    |> Option.value ~default:[]
    |> List.filter ~f:(test_one ~value ~time_delta ~delta)

(* = not (List.is_empty (test t event)) *)
let test' t event =
  match (event : Event_generator.event) with
  | Timer (interest, _t) ->
    begin
      match Hashtbl.find t.interests interest with
      | None
      | Some [] -> false
      | Some (_::_) -> true
    end

  | Probe (interest, _t, value) ->
    Hashtbl.find t.interests interest
    |> Option.value ~default:[]
    |> List.exists ~f:(test_one ~value ~time_delta:Time_ns.Span.zero ~delta:0)

  | Timer_path { interest; time_delta; time=_ } ->
    Hashtbl.find t.interests interest
    |> Option.value ~default:[]
    |> List.exists ~f:(test_one ~value:0 ~time_delta ~delta:0)

  | Probe_path { interest; time_delta; delta; value; time=_ } ->
    Hashtbl.find t.interests interest
    |> Option.value ~default:[]
    |> List.exists ~f:(test_one ~value ~time_delta ~delta)

let iter_events t events ~f =
  Event_generator.iter_events events ~f:(fun event ->
    match test t event with
    | [] -> ()
    | (_::_) as interests -> f event interests
  )

let iter_events_interests t events ~f =
  Event_generator.iter_events events ~f:(fun event ->
    List.iter (test t event) ~f:(f event)
  )

let raw_interests t = Hashtbl.keys t.interests
