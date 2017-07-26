open Core
open Core_profiler
open Core_profiler_disabled

(* (As per Path.readme,) This does not have the full power of regular expressions
   Notably, the same point may not appear in a path twice, except for when its
   second appearance is the last point in the path. *)

(* The [current_session] of all groups is initialised to 0; and the session of
   all points is initialised to -1.

   To save memory and avoid allocations, junk values (-1) are stored in
   [point_state.at_index], [point_state.value] and [point_state.time] at startup.

   The mis-match in [session] between group and point prevents the junk values in
   [at_index], [value] and [time] from ever being used. *)

type group_state =
  { mutable current_session : int
    (* The number of calls to [at] (on points in this group) in this session *)
  ; mutable session_at_count : int
  }

type point_state =
  { mutable time : Time_ns.t
  ; mutable session : int
    (* [at_index]: when this point was marked in this session
       (i.e., set to [group_state.session_at_count] when marked) *)
  ; mutable at_index : int
  ; mutable value : int
  }

type t =
  { id_map : Reader.Header.t
  ; epoch : Profiler_epoch.t
    (* Read only tables, but the cells are mutable records. *)
  ; group_state : (group_state, read) Id_table.t
  ; point_state : (point_state, read) Id_table.t
    (* Interests at this point, or interests in paths ending at this point. *)
  ; interests : (Probe_id.t Interest.Raw.t array, read) Id_table.t
  ; buffer : (read, Iobuf.no_seek) Iobuf.t
  }

type timer_path =
  { interest : Probe_id.t Interest.Raw.t
  ; time : Time_ns.t
  ; time_delta : Time_ns.Span.t
  }
[@@deriving sexp, compare]

type probe_path =
  { interest : Probe_id.t Interest.Raw.t
  ; time : Time_ns.t
  ; time_delta : Time_ns.Span.t
  ; value : int
  ; delta : int
  }
[@@deriving sexp, compare]

type event =
  | Timer       of Probe_id.t Interest.Raw.t * Time_ns.t
  | Probe       of Probe_id.t Interest.Raw.t * Time_ns.t * int
  | Timer_path  of timer_path
  | Probe_path  of probe_path
[@@deriving sexp, compare]

let event_time = function
  | Timer      (_, time)     -> time
  | Probe      (_, time, _)  -> time
  | Timer_path { time; _ }   -> time
  | Probe_path { time; _ }   -> time

let create epoch id_map interests buffer =
  let interests_lookup =
    Reader.Header.create_table id_map ~groups:false Interest.Raw.I.Set.empty
  in
  List.iter interests ~f:(fun (interest : Probe_id.t Interest.Raw.t) ->
    let point =
      match interest with
      | Single id -> id
      | Group_point (_grp, id) -> id
      | Group_path (_grp, path) -> path.last
    in

    Id_table.find_exn interests_lookup point
    |> Fn.flip Set.add interest
    |> Id_table.set_exn interests_lookup point
  );
  let interests_lookup =
    Id_table.map ~f:(fun _id set -> Set.to_array set) interests_lookup
  in

  { id_map
  ; epoch
  ; group_state = Id_table.filter_map id_map ~f:(fun _id header_item ->
      match header_item with
      | Reader.Header.Item.Group _ ->
        Some
          { current_session = 0
          ; session_at_count = 0
          }
      | Single _
      | Group_point _ -> None
    )
  ; point_state = Id_table.filter_map id_map ~f:(fun _id header_item ->
      match header_item with
      | Reader.Header.Item.Group_point _ ->
        Some
          { session = -1
          ; time = Time_ns.epoch
          ; value = -1
          ; at_index = 0
          }
      | Single _
      | Group _ -> None
   )
  ; interests = Id_table.read_only interests_lookup
  ; buffer = Iobuf.no_seek (Iobuf.read_only buffer)
  }

let at_group_point t ~point_id ~group_id time value =
  let group_state = Id_table.find_exn t.group_state group_id in
  let point_state = Id_table.find_exn t.point_state point_id in

  point_state.session <- group_state.current_session;
  point_state.at_index <- group_state.session_at_count;
  point_state.time <- time;
  Option.iter value ~f:(fun value -> point_state.value <- value);

  group_state.session_at_count <- group_state.session_at_count + 1

let test_path t group_state (path : Probe_id.t Path.t) =
  let current_session = group_state.current_session in

  (* [last_at_index] is the [at_index] of the previous point that was considered.
     It's used to check whether a point was marked before the previous point
     (it walks from the last point to the first), and whether an edge was direct. *)
  let test point last_at_index =
    match point with
    | Path.Direct_point id ->
      let point_state = Id_table.find_exn t.point_state id in
      let at_index = point_state.at_index in

      if (point_state.session = current_session) && (at_index = last_at_index - 1)
      then `Point_ok at_index
      else `No_match

    | Point id ->
      let point_state = Id_table.find_exn t.point_state id in
      let at_index = point_state.at_index in

      if (point_state.session = current_session) && (at_index < last_at_index)
      then `Point_ok at_index
      else `No_match
  in

  (* Note that we walk from the last point to the first. *)
  let rec loop points last_at_index =
    match points with
    | [] ->
      begin
        match test path.first last_at_index with
        | `Point_ok _ -> true
        | `No_match   -> false
      end

    | pt :: points ->
      begin
        match test pt last_at_index with
        | `Point_ok at_index -> loop points at_index
        | `No_match -> false
      end
  in

  loop path.Path.rest_rev group_state.session_at_count

let iter_events t ~f =
  Reader.iter_short_messages t.buffer t.epoch t.id_map ~f:(fun message ->
    let id = Reader.Short_message.id message in

    let header_item = Id_table.find_exn t.id_map id in
    (* This variable represents both /whether/ this is a group point, and if so,
        /what/ its group id is *)
    let group_point_parent =
      match header_item with
      | Group_point { parent; _ } -> Some parent
      | Single _
      | Group _ -> None
    in
    let group_state = lazy (
      Option.value_exn group_point_parent
      |> Id_table.find_exn t.group_state
    )
    in

    let at_group_point' time value =
      Option.iter group_point_parent ~f:(fun parent ->
        at_group_point t ~point_id:id ~group_id:parent time value
      )
    in

    let interests = Id_table.find t.interests id in

    match message with
    | Timer (id, time) ->
      Array.iter (Option.value_exn interests) ~f:(fun interest ->
        match interest with
        | Single id2
        | Group_point (_, id2) ->
          assert (id = id2);
          f (Timer (interest, time))

        | Group_path (_gp, path) ->
          assert (id = Path.last path);
          if test_path t (Lazy.force group_state) path
          then begin
            let first_point_state = Id_table.find_exn t.point_state (Path.first path) in
            let time_delta = Time_ns.diff time first_point_state.time in
            f (Timer_path { interest; time; time_delta })
          end
      );
      at_group_point' time None

    | Probe (id, time, value) ->
      Array.iter (Option.value_exn interests) ~f:(fun interest ->
        match interest with
        | Single id2
        | Group_point (_, id2) ->
          assert (id = id2);
          f (Probe (interest, time, value))

        | Group_path (_gp, path) ->
          assert (id = Path.last path);
          if test_path t (Lazy.force group_state) path
          then begin
            let first_point_state = Id_table.find_exn t.point_state (Path.first path) in
            let time_delta = Time_ns.diff time first_point_state.time in
            let delta = value - first_point_state.value in
            f (Probe_path { interest; time; time_delta; delta; value })
          end
      );
      at_group_point' time (Some value)

    | Group_reset (id, _) ->
      let group_state = Id_table.find_exn t.group_state id in
      group_state.current_session <- group_state.current_session + 1
  )

let%test_module "iter_group_events" = (module struct
  module Protocol = Core_profiler.Protocol

  let to_id = Probe_id.of_int_exn
  let to_time_delta = Time_ns.Span.of_int_sec
  let to_time n = Profiler_epoch.add Protocol.Writer.epoch (to_time_delta n)

  let header =
    let open Protocol in
    protect
      ~finally:Buffer.Unsafe_internals.reset
      ~f:(fun () ->
        Writer.Unsafe_internals.write_epoch ();
        Writer.write_new_group (to_id 0) "group" (Probe_type.Probe Profiler_units.Seconds);
        List.iter
          [ ("a", 1); ("b", 2); ("c", 3); ("d", 4); ("e", 5); ("f", 6); ("g", 7) ]
          ~f:(fun (name, id) ->
            Writer.write_new_group_point ~id:(to_id id) ~group_id:(to_id 0) name [||]
          );
        Writer.Unsafe_internals.write_end_of_header ();

        Buffer.get_header_chunk ()
        |> Reader.consume_header
        |> snd
      )

  let name_map = Util.Name_map.of_id_map header
  let header_group = Map.find_exn name_map.groups "group"

  let to_path s =
    Path.string_t_of_string s
    |> Option.value_exn
    |> Fn.flip Path.lookup_ids header_group

  let to_path_int s = Interest.Raw.Group_path (to_id 0, to_path s)

  let run_case ats interests =
    protect
      ~finally:Protocol.Buffer.Unsafe_internals.reset
      ~f:(fun () ->
        let at id n =
          Protocol.Writer.write_probe_at (to_id id) (to_time n) n
        in
        String.to_list ats
        |> List.iteri ~f:(fun n c ->
          match c with
          | 'a' -> at 1 n
          | 'b' -> at 2 n
          | 'c' -> at 3 n
          | 'd' -> at 4 n
          | 'e' -> at 5 n
          | 'f' -> at 6 n
          | 'g' -> at 7 n
          | 'r' -> Protocol.Writer.write_group_reset (to_id 0) (to_time n)
          | ' ' -> ()
          | _ -> failwith "Bad test case"
        );

        let buffer =
          match Protocol.Buffer.get_chunks () with
          | [x] -> x
          | _ -> failwith "expected one chunk"
        in

        let ev_gen = create Protocol.Writer.epoch header interests buffer in
        let events_rev = ref [] in
        iter_events ev_gen ~f:(fun x -> events_rev := x :: !events_rev);

        List.rev !events_rev
      )

  let to_event interest value delta =
    Probe_path
      { interest
      ; time = to_time value; time_delta = to_time_delta delta
      ; value; delta
      }

  let%test_unit "multiple simultaneous events" =
    [%test_eq: event list]
      (run_case "abc" [ to_path_int "a..c"; to_path_int "b,c" ])
      [ to_event (to_path_int "b,c") 2 1; to_event (to_path_int "a..c") 2 2 ]

  let%test_unit "reset" =
    [%test_eq: event list] (run_case "aaa r ccc" [ to_path_int "a..c" ]) []

  let%test_unit "directness" =
    [%test_eq: event list]
      (run_case "cd d dc r c   d ced r ced" [ to_path_int "c,d" ])
      [ to_event (to_path_int "c,d") 1 1; to_event (to_path_int "c,d") 14 4 ]

  let%test_unit "repeated" =
    let p = to_path_int "a,a" in
    [%test_eq: event list]
      (run_case "aaaaa r a" [ p ])
      [ to_event p 1 1; to_event p 2 1; to_event p 3 1; to_event p 4 1 ]

  (* TEST_UNIT "multiple simultaneous events" =
   *   <:test_eq< event list >>
   *     (run_case "abc" [ to_path_int "a,c"; to_path_int "b.c" ])
   *     [ to_event (to_path_int "b.c") 2 1; to_event (to_path_int "a,c") 2 2 ]
   *
   * TEST_UNIT "reset" =
   *   <:test_eq< event list >> (run_case "aaa r ccc" [ to_path_int "a,c" ]) []
   *
   * TEST_UNIT "directness" =
   *   <:test_eq< event list >>
   *     (run_case "cd d dc r c   d ced r ced" [ to_path_int "c.d" ])
   *     [ to_event (to_path_int "c.d") 1 1; to_event (to_path_int "c.d") 14 4 ]
   *
   * TEST_UNIT "repeated" =
   *   let p = to_path_int "a.a" in
   *   <:test_eq< event list >>
   *     (run_case "aaaaa r a" [ p ])
   *     [ to_event p 1 1; to_event p 2 1; to_event p 3 1; to_event p 4 1 ] *)
end)
