open Core
open Core_profiler
open Core_profiler_disabled


module Header = struct
  module Item = struct
    type single =
      { name : string
      ; spec : Probe_type.t
      }
    type group =
      { name : string
      ; points_spec : Probe_type.t
      ; children : Probe_id.t list
      }
    type group_point =
      { name : string
      ; parent : Probe_id.t
      ; sources : Probe_id.t list
      }

    type t =
      | Single of single
      | Group of group
      | Group_point of group_point

    let name = function
      | Single m -> m.name
      | Group p -> p.name
      | Group_point pp -> pp.name
  end

  type t = (Item.t, read) Id_table.t

  let find_exn = Id_table.find_exn

  let find_single_exn t id =
    match find_exn t id with
    | Item.Single m -> m
    | _ -> failwithf !"Id %{Probe_id} does not refer to a single" id ()

  let find_group_exn t id =
    match find_exn t id with
    | Item.Group p -> p
    | _ -> failwithf !"Id %{Probe_id} does not refer to a Group" id ()

  let find_group_point_exn t id =
    match find_exn t id with
    | Item.Group_point pp -> pp
    | _ -> failwithf !"Id %{Probe_id} does not refer to a Group_point" id ()

  let get_parent_id_exn t id =
    match find_exn t id with
    | Item.Group_point pp -> pp.parent
    | _ -> failwithf !"Id %{Probe_id} does not refer to a Group_point" id ()

  let get_parent_exn t id = find_group_exn t (get_parent_id_exn t id)

  let get_name_exn t ?with_group id =
    match find_exn t id with
    | Item.Single { Item.name; _ } -> name
    | Item.Group { Item.name; _ } -> name
    | Item.Group_point { Item.name; parent; _ } ->
      begin
        match with_group with
        | Some sep ->
          let { Item.name = group_name; points_spec = _; _ } = find_group_exn t parent in
          group_name ^ sep ^ name
        | None -> name
      end

  let get_spec_exn t id =
    match find_exn t id with
    | Item.Single { spec; _ } -> spec
    | Item.Group { points_spec; _ } -> points_spec
    | Group_point { parent; _ } ->
      let { Item.points_spec; _ } = find_group_exn t parent in
      points_spec

  let get_units_exn t id =
    match get_spec_exn t id with
    | Probe_type.Probe units -> units
    | Probe_type.Timer ->
      failwithf !"Id %{Probe_id} does not refer to something with units" id ()

  let create_table
        t
        ?(singles=true) ?(groups=true) ?(group_points=true)
        ?(timers=true) ?(probes=true)
        empty =

    Id_table.filter_map t ~f:(fun id header_item ->
      let spec_ok =
        match get_spec_exn t id with
        | Probe_type.Timer -> timers
        | Probe_type.Probe _ -> probes
      in
      let item_type_ok =
        match header_item with
        | Item.Single _       -> singles
        | Item.Group _        -> groups
        | Item.Group_point _  -> group_points
      in

      if spec_ok && item_type_ok
      then Some empty
      else None
    )
end

let consume_header buffer =
  let module HP = Header_protocol in
  let module HP_MsgT = Header_protocol.Message_type_and_errors in

  let add_exn map id data =
    match Map.find map id with
    | Some existing ->
      failwithf !"Duplicate Id %{Probe_id} (%s, %s)"
        id (Header.Item.name existing) (Header.Item.name data) ()
    | None ->
      Map.set map ~key:id ~data
  in

  let add_point_to_group map ~group_id ~point_id =
    match Map.find map group_id with
    | Some (Header.Item.Group p) ->
      let p = { p with children = point_id :: p.children } in
      Map.set map ~key:group_id ~data:(Header.Item.Group p)

    | Some (Header.Item.Single _ | Group_point _)
    | None ->
      failwith "Tried to add a point to something that isn't a group"
  in

  let check_sources_parent map ~point_id ~group_id sources =
    List.iter sources ~f:(fun source_id ->
      match Map.find map source_id with
      | Some (Header.Item.Group_point pp) ->
        if pp.parent <> group_id then
          failwithf
            !"Point %{Probe_id} of group %{Probe_id} references source point \
              %{Probe_id} not belonging to the same group"
            point_id group_id source_id ()
      | Some (Header.Item.Single _ | Header.Item.Group _)
      | None ->
        failwithf
          !"Point %{Probe_id} of group %{Probe_id} references source point \
            %{Probe_id} that is not a group point"
          point_id group_id source_id ()
    )
  in

  let get_message_buffer buffer =
    let b = Iobuf.sub_shared buffer in
    HP.skip_message buffer;
    b
  in

  let rec scan epoch map =
    let this_message_buffer = get_message_buffer buffer in
    let HP_MsgT.T packed_type = HP.get_message_type this_message_buffer in
    let message = HP.of_iobuf this_message_buffer ~trusted:packed_type in

    match packed_type with
    | HP_MsgT.New_single -> HP.New_single.(
      let spec = get_spec message in
      let map =
        add_exn map
          (get_id message)
          (Header.Item.Single { name = get_name message; spec })
      in
      scan epoch map
    )
    | HP_MsgT.New_group -> HP.New_group.(
      let points_spec = get_spec message in
      let group_spec =
        { name = get_name message
        ; Header.Item.points_spec
        ; children = []
        }
      in
      let map =
        add_exn map
          (get_id message)
          (Header.Item.Group group_spec)
      in
      scan epoch map
    )
    | HP_MsgT.New_group_point -> HP.New_group_point.(
      let sources =
        let count = get_sources_count message in
        List.init ~f:(fun i -> get_sources_source_id message ~count ~index:i) count
      in
      let point_id = get_id message in
      let group_id = get_group_id message in
      check_sources_parent map ~point_id ~group_id sources;

      let metadata =
        Header.Item.Group_point
          { name = get_name message
          ; parent = group_id
          ; sources
          }
      in
      let map = add_exn map point_id metadata in
      let map = add_point_to_group map ~group_id ~point_id in
      scan epoch map
    )
    | HP_MsgT.Epoch -> HP.Epoch.(
      match epoch with
      | Some epoch ->
        failwithf !"Header contained two epochs: %{Profiler_epoch}, %{Profiler_epoch}"
          epoch (get_epoch message) ()
      | None ->
        scan (Some (get_epoch message)) map
    )

    | HP_MsgT.End_of_header ->
      (epoch, map)

    | HP_MsgT.Need_more_data ->
      failwith "Invalid header (truncated)"
    | HP_MsgT.Invalid_message_type_or_subtype ->
      failwith "Invalid header (bad message type)"
    | HP_MsgT.Message_length_too_short ->
      failwith "Invalid header (message length too short)"
  in

  let (epoch, map) = scan None Probe_id.Map.empty in

  let epoch = Option.value_exn ~message:"Header did not contain an epoch" epoch in
  let table = Id_table.init_from_map map ~f:(fun _id item -> item) in

  (epoch, table)

let%test_unit "read_header" =
  let module Buffer = Protocol.Buffer in
  let module Writer = Protocol.Writer in

  let module HI = Header.Item in

  let to_id = Probe_id.of_int_exn in

  protect
    ~finally:Buffer.Unsafe_internals.reset
    ~f:(fun () ->
      Writer.Unsafe_internals.write_epoch ();
      Writer.write_new_single (to_id 3) "timer3" Probe_type.Timer;
      Writer.write_new_group (to_id 2) "group2" (Probe_type.Probe Profiler_units.Seconds);
      Writer.write_new_group (to_id 1) "group1" (Probe_type.Timer);
      Writer.write_new_group_point
        ~id:(to_id 4) ~group_id:(to_id 1) "group1point4" [||];
      Writer.write_new_group_point
        ~id:(to_id 5) ~group_id:(to_id 2) "group2point5" [||];
      Writer.write_new_group_point
        ~id:(to_id 6) ~group_id:(to_id 2) "group2point6" [|to_id 5|];
      Writer.write_new_single (to_id 0) "probe0" (Probe_type.Probe Profiler_units.Words);
      Writer.write_new_single (to_id 8) "timer8" Probe_type.Timer;
      Writer.write_new_group_point
        ~id:(to_id 7) ~group_id:(to_id 2) "group2point7" [|to_id 5; to_id 6|];
      Writer.Unsafe_internals.write_end_of_header ();

      let (epoch2, id_map) = consume_header (Buffer.get_header_chunk ()) in
      let id_map_alist = Id_table.to_alist id_map in
      [%test_pred: Profiler_epoch.t] (fun a -> a = Writer.epoch) epoch2;
      [%test_eq: int] (List.length id_map_alist) 9;
      let expect =
        [ (to_id 0, HI.Single { name = "probe0"; spec = Probe_type.Probe Profiler_units.Words })
        ; ( to_id 1
          , HI.Group { name = "group1"; points_spec = Probe_type.Timer; children = [to_id 4] }
          )
        ; ( to_id 2
          , HI.Group
              { name = "group2"
              ; HI.points_spec = Probe_type.Probe Profiler_units.Seconds
              ; children = [to_id 7; to_id 6; to_id 5]
              }
          )
        ; (to_id 3, HI.Single { name = "timer3"; spec = Probe_type.Timer })
        ; (to_id 4, HI.Group_point { name = "group1point4"; parent = to_id 1; sources = [] })
        ; (to_id 5, HI.Group_point { name = "group2point5"; parent = to_id 2; sources = [] })
        ; ( to_id 6
          , HI.Group_point { name = "group2point6"; parent = to_id 2; sources = [to_id 5] }
          )
        ; ( to_id 7
          , HI.Group_point
              { name = "group2point7"; parent = to_id 2; sources = [to_id 5; to_id 6] }
          )
        ; (to_id 8, HI.Single { name = "timer8"; spec = Probe_type.Timer})
        ]
      in
      assert (id_map_alist = expect)
  )

(* There's scope for making a zero-copy version of this *)
module Short_message = struct
  module Header = Protocol.Short_header

  type t =
    | Timer of Probe_id.t * Time_ns.t
    | Probe of Probe_id.t * Time_ns.t * int
    | Group_reset of Probe_id.t * Time_ns.t
  [@@deriving sexp, compare]

  let id = function
    | Timer (id, _) -> id
    | Probe (id, _, _) -> id
    | Group_reset (id, _) -> id

  let time = function
    | Timer (_, time) -> time
    | Probe (_, time, _) -> time
    | Group_reset (_, time) -> time
end


let consume_short_message buffer epoch header =
  let module SM = Short_message in
  let module HI = Header.Item in

  let remaining = Iobuf.length buffer in
  if remaining = 0 then
    failwith "Invalid short message: empty buffer"
  else if remaining < 8 then
    failwith "Invalid short message: truncated"
  else begin

    (* fields common to read_timer, probe and group_reset *)
    let sm_header = Iobuf.Peek.int64_le buffer ~pos:0 in
    let sm_id = SM.Header.unpack_id sm_header in
    let sm_time = SM.Header.unpack_time epoch sm_header in

    let read_by_spec = function
      | Probe_type.Timer ->
        Iobuf.unsafe_advance buffer 8;
        SM.Timer (sm_id, sm_time)
      | Probe_type.Probe _ ->
        if remaining < 16 then
          failwith "Invalid short message: truncated"
        else begin
          let value = Iobuf.Peek.int64_le buffer ~pos:8 in
          Iobuf.unsafe_advance buffer 16;
          SM.Probe (sm_id, sm_time, value)
        end
    in
    let read_group_reset () =
      Iobuf.unsafe_advance buffer 8;
      SM.Group_reset (sm_id, sm_time)
    in

    match Header.find_exn header sm_id with
    | HI.Group _ ->
      read_group_reset ()
    | HI.Single { spec; _ } ->
      read_by_spec spec

    | HI.Group_point { parent; _ } ->
      let { HI.points_spec; _ } = Header.find_group_exn header parent in
      read_by_spec points_spec

  end

let%test_unit "consume_short_message" =
  let module Buffer = Protocol.Buffer in
  let module Writer = Protocol.Writer in

  protect
    ~finally:Buffer.Unsafe_internals.reset
    ~f:(fun () ->
      let to_id = Probe_id.of_int_exn in
      let time_past_epoch x = Profiler_epoch.add Writer.epoch (Time_ns.Span.of_int_ns x) in

      Writer.Unsafe_internals.write_epoch ();
      Writer.write_new_single (to_id 1) "timer" Probe_type.Timer;
      Writer.write_new_single (to_id 2) "probe" (Probe_type.Probe Profiler_units.Seconds);
      Writer.write_new_group (to_id 3) "timer-group" Probe_type.Timer;
      Writer.write_new_group_point
        ~group_id:(to_id 3) ~id:(to_id 4) "timer-group-point" [||];
      Writer.write_new_group
        (to_id 5) "probe-group" (Probe_type.Probe Profiler_units.Int);
      Writer.write_new_group_point
        ~group_id:(to_id 5) ~id:(to_id 6) "probe-group-point" [||];
      Writer.Unsafe_internals.write_end_of_header ();

      Writer.write_timer_at (to_id 1) (time_past_epoch 100);
      Writer.write_probe_at (to_id 2) (time_past_epoch 200) 22;
      Writer.write_group_reset (to_id 3) (time_past_epoch 300);
      Writer.write_timer_at (to_id 4) (time_past_epoch 400);
      Writer.write_probe_at (to_id 6) (time_past_epoch 600) 66;

      let (epoch, header) = consume_header (Buffer.get_header_chunk ()) in
      let short_messages_chunk =
        match Buffer.get_chunks () with
        | [x] -> Iobuf.sub_shared x
        | _ -> assert false
      in

      let read () = consume_short_message short_messages_chunk epoch header in

      [%test_eq: Short_message.t] (read ()) (Timer (to_id 1, time_past_epoch 100));
      [%test_eq: Short_message.t] (read ()) (Probe (to_id 2, time_past_epoch 200, 22));
      [%test_eq: Short_message.t] (read ()) (Group_reset (to_id 3, time_past_epoch 300));
      [%test_eq: Short_message.t] (read ()) (Timer (to_id 4, time_past_epoch 400));
      [%test_eq: Short_message.t] (read ()) (Probe (to_id 6, time_past_epoch 600, 66));
    )

let fold_short_messages buffer epoch header ~init ~f =
  Iobuf.protect_window_and_bounds (Iobuf.no_seek buffer) ~f:(fun buffer ->
    let rec loop accum =
      if Iobuf.is_empty buffer
      then accum
      else begin
        let message = consume_short_message buffer epoch header in
        loop (f accum message)
      end
    in
    loop init
  )

let iter_short_messages buffer epoch header ~f =
  Iobuf.protect_window_and_bounds (Iobuf.no_seek buffer) ~f:(fun buffer ->
    while not (Iobuf.is_empty buffer) do
      f (consume_short_message buffer epoch header)
    done
  )

let iteri_short_messages buffer epoch header ~f =
  Iobuf.protect_window_and_bounds (Iobuf.no_seek buffer) ~f:(fun buffer ->
    let i = ref 0 in
    while not (Iobuf.is_empty buffer) do
      f !i (consume_short_message buffer epoch header);
      incr i
    done
  )

let map_file filename =
  let file_length fd =
    let current_pos = Unix.lseek fd ~mode:Unix.SEEK_CUR Int64.zero in
    let length = Unix.lseek fd ~mode:Unix.SEEK_END Int64.zero in
    ignore (Unix.lseek fd ~mode:Unix.SEEK_SET current_pos : int64);

    length |> Int64.to_int |> Option.value_exn
  in

  Unix.with_file filename ~mode:[Unix.O_RDONLY] ~f:(fun fd ->
    let map = Bigstring.map_file ~shared:false fd (file_length fd) in
    Iobuf.of_bigstring map
  )
