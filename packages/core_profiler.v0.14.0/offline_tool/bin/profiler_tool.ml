open! Core
open Poly
open Core_profiler
open Core_profiler_disabled
open Core_profiler_offline_tool.Std

let iobuf_file_flag () =
  Command.Spec.(
    begin
      match Sys.file_exists Protocol.default_output_filename with
      | `Yes -> optional_with_default Protocol.default_output_filename string
      | `No | `Unknown -> required string
    end
    |> map_flag ~f:Reader.map_file
    |> flag "-filename" ~doc:(sprintf "%s core_profiler file to open"
                                Protocol.default_output_filename)
  )

let lookup_coerce_interests id_map interests =
  let name_map = Name_map.of_id_map id_map in
  List.map interests ~f:(fun interest ->
    Interest.lookup_ids interest name_map
    |> Fn.flip Interest.coerce_interval_units id_map
  )

let or_default_interests id_map interests =
  if List.is_empty interests
  then Interest.default_interests id_map
  else interests

let build_filter id_map interests =
  let filter = Filter.create id_map in
  List.iter interests ~f:(Filter.add_interest filter);
  Filter.read_only filter

module Log_command = struct
  type context =
    | Lines of int
    | Timespan of Time_ns.Span.t

  let context_of_string x =
    match int_units_of_string x with
    | (value, Profiler_units.Nanoseconds)  when value > 0 ->
      Timespan (Time_ns.Span.of_int_ns value)
    | (value, Profiler_units.Seconds)      when value > 0 ->
      Timespan (Time_ns.Span.of_int_sec value)
    | (value, Profiler_units.Int)        when value > 0 ->
      Lines value
    | _ -> failwithf "Invalid context string %s" x ()

  let context_arg_type = Command.Spec.Arg_type.create context_of_string


  let raw_interest_name id_map interest =
    match (interest : Probe_id.t Interest.Raw.t) with
    | Single id
    | Group_point (_, id) ->
      Reader.Header.get_name_exn ~with_group:":" id_map id
    | Group_path (gp, path) ->
      Reader.Header.get_name_exn id_map gp ^ ":" ^ Path.id_t_to_string path id_map

  let max_name_length id_map filter =
    List.fold
      (Filter.raw_interests filter)
      ~init:0
      ~f:(fun max interest ->
        let name = raw_interest_name id_map interest in
        Int.max (String.length name) max
      )

  let display_event id_map event ansi_attrs name_length =
    let printf = Console.Ansi.printf in
    let name = raw_interest_name id_map in
    let units ri =
      match (ri : Probe_id.t Interest.Raw.t) with
      | Single id
      | Group_point (id, _)
      | Group_path (id, _) ->
        Reader.Header.get_units_exn id_map id
    in

    match (event : Event_generator.event) with
    | Timer (interest, time) ->
      printf ansi_attrs !"%s  %-*s  at\n"
        (time_ns_to_ofday_string time) name_length (name interest)
    | Probe (interest, time, value) ->
      printf ansi_attrs !"%s  %-*s  v  %8s\n"
        (time_ns_to_ofday_string time) name_length (name interest)
        (Profiler_units.format_int (units interest) value)
    | Timer_path { interest; time; time_delta } ->
      printf ansi_attrs !"%s  %-*s  dt %8s\n"
        (time_ns_to_ofday_string time) name_length (name interest)
        (span_to_string time_delta)
    | Probe_path { interest; time; value; time_delta; delta } ->
      let units = units interest in
      printf ansi_attrs !"%s  %-*s  dt %8s  dv %8s  v %8s\n"
        (time_ns_to_ofday_string time) name_length (name interest)
        (span_to_string time_delta)
        (Profiler_units.format_int units delta)
        (Profiler_units.format_int units value)


  let display_all id_map filter events =
    let name_length = max_name_length id_map filter in
    Filter.iter_events filter events ~f:(fun event _ ->
      display_event id_map event [] name_length
    )

  let display_near id_map filter events near_filter near_context =
    let (dequeue, prune) =
      match near_context with
      | Lines near_lines ->
        let dequeue = Deque.create ~initial_length:near_lines ~never_shrink:true () in

        let prune _time () =
          let drop = Deque.length dequeue - near_lines in
          if drop > 0 then Deque.drop_back ~n:drop dequeue
        in

        (dequeue, prune)

      | Timespan near_tspan ->
        let dequeue = Deque.create () in
        let prune time () =
          let should_drop event_time =
            Time_ns.Span.(>) (Time_ns.diff time event_time) near_tspan
          in

          let rec loop () =
            match Deque.peek_back dequeue with
            | Some e when should_drop (Event_generator.event_time e) ->
              Deque.drop_back dequeue;
              loop ()
            | _ -> ()
          in
          loop ()
        in

        (dequeue, prune)
    in

    (* matching_event : an event matched! set the state
                        boolean argument specifies whether the event that matched will
                        itself be displayed
       next_event     : on every event: update the state and return
                        'should this event be displayed' *)
    let ( (matching_event : Time_ns.t -> bool -> unit -> unit)
        , (next_event     : Time_ns.t -> unit -> bool)
        ) =
      match near_context with
      | Lines near_lines ->
        let state = ref 0 in

        let matching_event _time matched_will_be_displayed () =
          state := near_lines + (if matched_will_be_displayed then 1 else 0)
        in
        let next_event _time () =
          let show_this_event = !state > 0 in
          state := Int.max 0 (!state - 1);
          show_this_event
        in

        (matching_event, next_event)

      | Timespan near_tspan ->
        let state = ref Time_ns.epoch in

        let matching_event time _mwbd () =
          state := time
        in
        let next_event time () =
          Time_ns.Span.(<) (Time_ns.diff time !state) near_tspan
        in

        (matching_event, next_event)
    in

    let name_length = max_name_length id_map filter in
    let printed_once = ref false in

    Event_generator.iter_events events ~f:(fun event ->
      let time = Event_generator.event_time event in
      let matches_filter = Filter.test' filter event in
      let matches_near = Filter.test' near_filter event in

      if matches_near
      then begin
        if not (Deque.is_empty dequeue) then begin
          if !printed_once then print_string "---\n";
          Deque.iter'
            dequeue `back_to_front
            ~f:(fun m -> display_event id_map m [] name_length);
          Deque.clear dequeue
        end;

        matching_event time matches_filter ()
      end;

      if matches_filter
      then begin
        if next_event time () then begin
          printed_once := true;
          display_event id_map event (if matches_near then [ `Red ] else []) name_length
        end else begin
          Deque.enqueue_front dequeue event;
          prune time ()
        end
      end
    )

  let main interests buffer nears near_context =
    let (epoch, id_map) = Reader.consume_header buffer in
    let interests =
      lookup_coerce_interests id_map interests
      |> or_default_interests id_map
    in
    let filter = build_filter id_map interests in

    if List.is_empty nears
    then
      let raw_interests = Filter.raw_interests filter in
      let events = Event_generator.create epoch id_map raw_interests buffer in
      display_all id_map filter events
    else begin
      let nears = lookup_coerce_interests id_map nears in
      let near_filter = build_filter id_map nears in
      let raw_interests =
          Filter.raw_interests filter
        @ Filter.raw_interests near_filter
      in
      let events = Event_generator.create epoch id_map raw_interests buffer in
      display_near id_map filter events near_filter near_context
    end

  let readme () =
    String_extended.word_wrap ~soft_limit:75
      "The log may be filtered by specifying some INTERESTs. It may be further filtered \
       by asking only for entries near lines that match -near INTERESTs. \
       The amount of context shown is controlled by -context and may be specified as a \
       number of messages (e.g., -context 10, the default) or time \
       (e.g., -context 10ns)\n\n\
       See the  help-interests  subcommand for information on specifying the INTEREST."

  let command =
    Command.basic_spec
      ~summary:"Show the (full, or filtered) stream of events \
                (i.e., the log, having calculated paths deltas)."
      ~readme:readme
      Command.Spec.(
        empty
        +> Interest.list_arg
        +> iobuf_file_flag ()
        +> flag "-near" (listed Interest.arg_type)
             ~doc:"INTEREST Restrict to messages near where this condition is true"
        +> flag "-context" (optional_with_default (Lines 10) context_arg_type)
             ~doc:"10 How much context to show."
      )
      (fun interests buffer nears near_context () ->
         main interests buffer nears near_context
      )
end

module List_command = struct
  let prune_unused buffer epoch (id_map : Reader.Header.t) =
    let used = Id_table.create' id_map false in

    Reader.iter_short_messages
      buffer epoch id_map
      ~f:(fun message ->
        match message with
        | Timer (id, _)
        | Probe (id, _, _) ->
          Id_table.set_exn used id true
        | Group_reset _ ->
          (* groups are only marked as used if their points are used *)
          ()
      );

    (* Mark groups as used if a constituent point was *)
    Id_table.iter
      used
      ~f:(fun id id_used ->
        if id_used then begin
          match Reader.Header.find_exn id_map id with
          | Single _ -> ()
          | Group_point { parent; _ } ->
            Id_table.set_exn used parent true

          | Group _ -> assert false
        end
      );

    Id_table.filter_map id_map ~f:(fun key data ->
      match (Id_table.find_exn used key, data) with
      | (false, _unused) -> None
      | (true, Single _) -> Some data
      | (true, Group_point _) -> Some data
      | (true, Group p) ->
        let children = List.filter p.children ~f:(Id_table.find_exn used) in
        Some (Group { p with children })
    )

  let main buffer prune () =
    let (epoch, id_map) = Reader.consume_header buffer in
    let id_map =
      if prune
      then prune_unused buffer epoch id_map
      else id_map
    in
    Id_table.fold_right
      id_map
      ~init:[]
      ~f:(fun accum id data ->
        match data with
        | Single { name;  spec } ->
          (id, name, Some spec) :: accum
        | Group { name; points_spec; children } ->
          (id, name, Some points_spec)
            ::
          List.fold
            children
            ~init:accum
            ~f:(fun accum id ->
              let { Reader.Header.Item.name; _ } =
                Reader.Header.find_group_point_exn id_map id
              in
              (id, "    " ^ name, None) :: accum
            )
        | Group_point _ ->
          (* group points are output immediately after the group *)
          accum
      )
    |> Ascii_table.(
      output
        ~oc:Out_channel.stdout
        [ Column.create ~align:Left "id"
            (fun (id, _, _) -> Probe_id.to_string id )
        ; Column.create ~align:Left "name" snd3
        ; Column.create ~align:Left "spec"
            (fun (_, _, spec) ->
               Option.map spec ~f:Probe_type.to_string
               |> Option.value ~default:""
            )
        ]
    )

  let command =
    Command.basic_spec
      ~summary:"List the singles and groups in a core-profiler file"
      Command.Spec.(
        empty
        +> iobuf_file_flag ()
        +> flag "-prune-unused" no_arg
            ~doc:" Don't show singles / groups / points that are never marked."
      )
      main
end

module Summary_command = struct
  module Selections = struct
    type t =
      { count       : bool
      ; sum         : bool
      ; mean        : bool
      ; min         : bool
      ; max         : bool
      ; stdev       : bool
      ; percentiles : Percent.t list
      }

    let default =
      { count       = true
      ; sum         = false
      ; mean        = true
      ; min         = true
      ; max         = true
      ; stdev       = true
      ; percentiles = List.map ~f:Percent.of_percentage [5.; 95.]
      }

    let need_rstats s       = s.count || s.sum || s.mean || s.min || s.max || s.stdev
    let need_order_stats s  = not (List.is_empty s.percentiles)

    let is_empty s = not (need_rstats s || need_order_stats s)
  end

  module Stats = struct
    type t =
      { rstats : Fstats.t option
      ; order_stats : Reservoir_sampling.t option
      ; mutable is_empty : bool
      }

    let create selections =
      { rstats =
          if Selections.need_rstats selections
          then Some (Fstats.create ())
          else None
      ; order_stats =
          if Selections.need_order_stats selections
          then Some (Reservoir_sampling.create ())
          else None
      ; is_empty = true
      }

    let update_in_place t v =
      t.is_empty <- false;
      Option.iter t.rstats ~f:(fun rstats ->
        Fstats.update_in_place rstats (float v)
      );
      Option.iter t.order_stats ~f:(fun order_stats ->
        Reservoir_sampling.add order_stats v
      )

    let is_empty t = t.is_empty

    let rstats_getter f t =
      f (Option.value_exn t.rstats ~message:"Didn't calculate RStats!")

    let count = rstats_getter Fstats.samples
    let sum   = rstats_getter Fstats.total
    let mean  = rstats_getter Fstats.mean
    let min   = rstats_getter Fstats.min
    let max   = rstats_getter Fstats.max
    let stdev = rstats_getter Fstats.stdev

    let percentile t x =
      Reservoir_sampling.percentile (Option.value_exn t.order_stats) x
  end

  type probe_path_stats =
    { time_delta_stats : Stats.t
    ; delta_stats : Stats.t
    }

  module Row = struct
    type t =
      { interest : Probe_id.t Interest.t
      ; what : string
      ; stats : Stats.t
      ; units : Profiler_units.t
      }

    let is_empty { stats; _ } = Stats.is_empty stats

    let float_formatter t x =
      match Float.iround_nearest x with
      | Some i -> Profiler_units.format_int t.units i
      | None -> ""

    let name { interest; _ } id_map = Interest.id_t_to_string interest id_map
    let what { what; _ } = what
    let count t = Profiler_units.format_int Profiler_units.Int (Stats.count t.stats)
    let sum   t = float_formatter t (Stats.sum   t.stats)
    let mean  t = float_formatter t (Stats.mean  t.stats)
    let min   t = float_formatter t (Stats.min   t.stats)
    let max   t = float_formatter t (Stats.max   t.stats)
    let stdev t = float_formatter t (Stats.stdev t.stats)
    let percentile t p =
      match Stats.percentile t.stats p with
      | Ok x -> Profiler_units.format_int t.units x
      | Error _ -> ""
  end

  let table_columns id_map (selections : Selections.t) =
    let add want name func acc =
      if want
      then Ascii_table.(Column.create ~align:Right name func) :: acc
      else acc
    in

    let percentiles =
      List.map selections.percentiles
        ~f:(fun p ->
          let name =
            Percent.to_percentage p
            |> Core.Float.to_string_hum ~decimals:1 ~strip_zero:true
            |> (fun s -> s ^ " %l")
          in
          let func = Fn.flip Row.percentile (Percent.to_mult p) in
          Ascii_table.(Column.create ~align:Right name func)
        )
    in

    let cols =
      percentiles
      |> add selections.stdev "stdev" Row.stdev
      |> add selections.max   "max"   Row.max
      |> add selections.min   "min"   Row.min
      |> add selections.mean  "mean"  Row.mean
      |> add selections.sum   "sum"   Row.sum
      |> add selections.count "count" Row.count
    in
    let (name, what) =
      Ascii_table.(
        ( Column.create ~align:Left "name" (Fn.flip Row.name id_map)
        , Column.create ~align:Left ""     Row.what
        )
      )
    in
    name :: what :: cols

  let row_sort_compare a b =
    let key row =
      match Interest.raw row.Row.interest with
      | Single id -> (1, [id])
      | Group_point (gp, id) -> (2, [gp; id])
      | Group_path (gp, path) ->
        let point_id = function
          | Path.Point id -> id
          | Path.Direct_point id -> id
        in
        let points =
          point_id path.first :: List.rev_map ~f:point_id path.rest_rev @ [path.last]
        in
        (3, gp :: points)
    in
    [%compare: int * Probe_id.t list] (key a) (key b)

  let singles_and_points_rows id_map stats_table =
    (* fold_right, producing rows sorted by Id increasing *)
    Hashtbl.fold stats_table
      ~init:[]
      ~f:(fun ~key:interest ~data:stats accum ->
        let units =
          Interest.spec interest id_map
          |> Probe_type.units
          |> Option.value_exn
        in
        { Row.interest; what = "v"; stats; units } :: accum
      )

  let timer_paths_rows tbl =
    Hashtbl.fold tbl
      ~init:[]
      ~f:(fun ~key:interest ~data:stats accum ->
        let units = Profiler_units.Nanoseconds in
        { Row.interest; what = "dt"; stats; units } :: accum
      )

  let probe_paths_rows id_map tbl =
    Hashtbl.fold tbl
      ~init:[]
      ~f:(fun ~key:interest ~data:{ delta_stats; time_delta_stats } accum ->
        assert (Stats.is_empty delta_stats = Stats.is_empty time_delta_stats);

        let units =
          Interest.spec interest id_map
          |> Probe_type.units
          |> Option.value_exn
        in
        { Row.interest;
          what = "dt";
          stats = time_delta_stats;
          units = Profiler_units.Nanoseconds
        }
        ::
        { Row.interest;
          what = "dv";
          stats = delta_stats;
          units
        }
        :: accum
      )

  let main interests buffer selections =
    let selections =
      if Selections.is_empty selections
      then Selections.default
      else selections
    in

    let (epoch, id_map) = Reader.consume_header buffer in
    let interests =
      lookup_coerce_interests id_map interests
      |> or_default_interests id_map
    in
    let filter = build_filter id_map interests in
    let raw_interests = Filter.raw_interests filter in
    let events = Event_generator.create epoch id_map raw_interests buffer in

    let probes = Interest.I.Table.create () in
    let timer_paths = Interest.I.Table.create () in
    let probe_paths = Interest.I.Table.create () in

    let new_stats () = Stats.create selections in
    List.iter interests ~f:(fun interest ->
      let spec = Interest.spec interest id_map in

      let res =
        match (Probe_type.is_probe spec, Interest.is_path interest) with
        | (true,  false) ->
          Hashtbl.add probes      ~key:interest ~data:(new_stats ())
        | (false, true)  ->
          Hashtbl.add timer_paths ~key:interest ~data:(new_stats ())
        | (true,  true)  ->
          Hashtbl.add probe_paths ~key:interest
            ~data:{ time_delta_stats = new_stats (); delta_stats = new_stats () }
        | (false, false) -> `Ok
      in

      ignore (res : [ `Ok | `Duplicate ])
    );

    Filter.iter_events_interests filter events ~f:(fun event interest ->
      match event with
      | Timer _ -> ()
      | Probe (_raw, _t, value) ->
        let s = Hashtbl.find_exn probes interest in
        Stats.update_in_place s value
      | Timer_path { time_delta; _ } ->
        let s = Hashtbl.find_exn timer_paths interest in
        Stats.update_in_place s (Time_ns.Span.to_int_ns time_delta)
      | Probe_path { time_delta; delta; _ } ->
        let { time_delta_stats; delta_stats } =
          Hashtbl.find_exn probe_paths interest
        in
        Stats.update_in_place time_delta_stats (Time_ns.Span.to_int_ns time_delta);
        Stats.update_in_place delta_stats delta
    );


    let rows =
      (   singles_and_points_rows id_map probes
        @ timer_paths_rows timer_paths
        @ probe_paths_rows id_map probe_paths
      )
      |> List.filter ~f:(fun r -> not (Row.is_empty r))
      |> List.sort ~compare:row_sort_compare
    in
    if rows = []
    then
      printf "No data to show.\n"
    else
      Ascii_table.output
        ~oc:Out_channel.stdout
        ~limit_width_to:150
        (table_columns id_map selections)
        rows

  let readme () =
    String_extended.word_wrap ~soft_limit:75
      "If no statistics (sum, mean, ...) are requested, a default selection will be \
       shown. \
       You can ask for multiple percentiles by passing the -percentile flag several \
       times.\n\n\
       The data may be filtered by specifying some INTERESTs; \
       see the  help-interests  subcommand."

  let percentile_arg_type =
    let open Command.Arg_type in
    comma_separated (map Command.Spec.float ~f:Percent.of_percentage) ~allow_empty:true

  let command =
    Command.basic_spec
      ~summary:"Compute basic statistics from a core-profiler file"
      ~readme
      Command.Spec.(
        empty
        +> Interest.list_arg
        +> iobuf_file_flag ()
        +> flag "-count"  no_arg ~doc:" Show the number of samples collected"
        +> flag "-sum"    no_arg ~doc:" ... the sum of the values"
        +> flag "-mean"   no_arg ~doc:" ... the arithmetic mean"
        +> flag "-min"    no_arg ~doc:" ... the minimum"
        +> flag "-max"    no_arg ~doc:" ... the maximum"
        +> flag "-stdev"  no_arg ~doc:" ... the standard deviation"
        +> flag "-median" no_arg ~doc:" ... the median (50th percentile)"
        +> flag "-percentile" ~aliases:["-%"] (listed percentile_arg_type)
             ~doc:"95 ... the Nth percentile (approximate, via reservoir sampling)"
      )
      (fun interests buffer count sum mean min max stdev median percentile_lists () ->
         let percentiles =
           ((if median then [Percent.of_percentage 50.] else [])
            @ List.concat percentile_lists)
           |> List.sort ~compare:Percent.compare
           |> List.remove_consecutive_duplicates ~equal:Percent.equal
         in
         main interests buffer { count; sum; mean; min; max; stdev; percentiles }
      )
end

module Plot_command = struct
  type variable =
    | Time
    | Value
    | Delta
    | Time_delta

  type plot =
    (* density / frequency of value (dep) vs variable's value (indep) *)
    | Density
    (* value of percentile of [plot_what] (dep), vs percentile (indep) *)
    | Percentiles

  let variable_argtype =
    choices_argtype "variable"
      [ ("time", Time)
      ; ("value", Value)
      ; ("delta", Delta)
      ; ("time_delta", Time_delta)

      ; ("dt", Time_delta)
      ; ("dv", Value)
      ]

  let variable_arg =
    Command.Spec.(
      (anon ("variable" %: variable_argtype))
    )

  let plot_percentiles =
    Command.Spec.(
      flag "-percentiles" no_arg
        ~doc:" Plot the value (dependent) against a range of percentiles (indep)"
      |>
      map ~f:(function
        | true -> Percentiles
        | false -> Density
      )
    )

  let plot_max_bins =
    Command.Spec.(
      flag "-bins" (optional_with_default 20 int)
        ~doc:"NUM Num of bins to plot."
    )

  let plot_log_scale =
    Command.Spec.(
      flag "-log-scale" no_arg
        ~doc:" Show relative densities on a log10 scale."
    )

  let get_range_reservoir filter events variable =
    (* The [Order_stats_reservoir_sampling] module, which is used in [Summary_command],
       also serves as a general purpose reservoir sampling tool. *)
    let reservoir = Reservoir_sampling.create () in

    let range = ref None in
    let update_range x =
      range :=
        match !range with
        | Some (low, high) -> Some (Int.min x low, Int.max x high)
        | None -> Some (x, x)
    in

    Filter.iter_events filter events ~f:(fun event _interests ->
      let var =
        match (variable, event) with
        | (Time, event) ->
          Event_generator.event_time event
          |> Time_ns.to_int_ns_since_epoch
          |> Option.some

        | (Value, (Probe (_, _, value) | Probe_path { value; _ })) ->
          Some value

        | (Delta, Probe_path { delta; _ }) ->
          Some delta

        | (Time_delta, (Timer_path { time_delta; _ } | Probe_path { time_delta; _ })) ->
          Some (Time_ns.Span.to_int_ns time_delta)

        | (Value, (Timer _ | Timer_path _))
        | (Delta, (Timer _ | Probe _ | Timer_path _))
        | (Time_delta, (Timer _ | Probe _)) ->
          None
      in

      Option.iter var ~f:(fun var ->
        Reservoir_sampling.add reservoir var;
        update_range var
      )
    );

    match !range with
    | Some range -> Some (range, reservoir)
    | None -> None

  let density_bars ~max_num_bins ~log_scale (min, max) reservoir_samples =
    let num_samples = List.length reservoir_samples in

    let tot_width_p1 = max - min + 1 in
    let num_bins =
      Int.min num_samples tot_width_p1
      |> Int.max 1
      |> Int.min max_num_bins
    in
    let bin_for_value value =
      (* Division rounding towards zero... *)
      (value - min) * num_bins / tot_width_p1
    in

    let bins = Array.create ~len:num_bins 0 in
    List.iter reservoir_samples ~f:(fun value ->
      let i = bin_for_value value in
      bins.(i) <- bins.(i) + 1
    );

    (* The edges are [min + (tot_width_p1 * i) / num_bins].
        The upper edge is open, so subtract one.
        [bin_for_value] will round down, so round the lower edge up and upper down *)
    let edges i =
      ( min + (tot_width_p1 * i       + num_bins - 1) / num_bins
      , min + (tot_width_p1 * (i + 1) - 1           ) / num_bins
      )
    in
    Array.mapi bins ~f:(fun i size ->
      (edges i,
       if log_scale then begin
         let x = float size *. 100_000. /. float num_samples in
         if x < 1.
         then 0.
         else Float.log10 x
       end
       else float size *. 100. /. float num_samples
      )
    )
    |> Array.to_list

  let percentile_bars (min, max) reservoir =
    List.range ~start:`inclusive 0 ~stop:`inclusive 100 ~stride:5
    |> List.map ~f:(function
      | 0   -> (0, min)
      | 100 -> (100, max)
      | p ->
        let p' = Float.of_int p /. 100. in
        let v = Reservoir_sampling.percentile_exn reservoir p' in
        (p, v)
    )

  let label_density_bars bars formatter =
    let bars =
      List.map bars ~f:(fun ((low, high), size) ->
        ((formatter low, formatter high), size)
      )
    in
    let (width_low, width_high) =
      List.fold
        bars
        ~init:(0, 0)
        ~f:(fun (low_max, high_max) ((low, high), _) ->
          (Int.max low_max (String.length low), Int.max high_max (String.length high))
        )
    in
    List.map bars ~f:(fun ((low, high), size) ->
      let label = sprintf "%*s -- %*s" width_low low width_high high in
      (label, size)
    )

  let label_percentile_bars bars formatter =
    (* (percentile, formatted value, value) *)
    let bars = List.map bars ~f:(fun (p, v) -> (p, formatter v, v)) in
    let width =
      List.fold bars ~init:0 ~f:(fun acc (_, fv, _) -> Int.max acc (String.length fv))
    in
    List.map bars ~f:(fun (p, fv, v) -> (sprintf "%3i %%l  %*s" p width fv, v))

  let shift_positive bars =
    (* min{ bar values, 0 } *)
    let min = List.fold bars ~init:0 ~f:(fun acc (_, v) -> Int.min acc v) in
    List.map bars ~f:(fun (l, v) -> (l, v - min))

  let check_sanity interest variable id_map =
    let var_is_delta =
      match variable with
      | Time | Value -> false
      | Time_delta | Delta -> true
    in
    let var_is_value =
      match variable with
      | Time | Time_delta -> false
      | Value | Delta -> true
    in
    let int_is_qty =
      match Interest.spec interest id_map with
      | Probe_type.Timer -> false
      | Probe_type.Probe _ -> true
    in

    if var_is_delta && not (Interest.is_path interest)
    then failwith "Interest must be a path to plot (time) deltas";

    if var_is_value && not int_is_qty
    then failwith "Interest must be a Probe to plot values or deltas"

  let main interest variable plot_perc max_num_bins log_scale buffer () =
    let (epoch, id_map) = Reader.consume_header buffer in
    let name_map = Name_map.of_id_map id_map in
    let filter = Filter.create id_map in
    let interest = Interest.lookup_ids interest name_map in

    check_sanity interest variable id_map;

    Filter.add_interest filter interest;
    let filter = Filter.read_only filter in

    let raw_interests = Filter.raw_interests filter in
    let events = Event_generator.create epoch id_map raw_interests buffer in

    let formatter =
      match variable with
      | Time -> (fun x -> time_ns_to_ofday_string (Time_ns.of_int_ns_since_epoch x))
      | Time_delta -> (fun x -> Profiler_units.format_int Profiler_units.Nanoseconds x)
      | Value
      | Delta ->
        let units =
          Interest.spec interest id_map
          |> Probe_type.units
          |> Option.value_exn
        in
        (fun x -> Profiler_units.format_int units x)
    in

    let (range, reservoir) =
      get_range_reservoir filter events variable
      |> Option.value_exn ~message:"No samples to plot"
    in

    let bars =
      match plot_perc with
      | Density ->
        let reservoir_samples =
          Reservoir_sampling.distribution reservoir
        in
        density_bars ~log_scale ~max_num_bins range reservoir_samples
        |> Fn.flip label_density_bars formatter

      | Percentiles ->
        percentile_bars range reservoir
        |> Fn.flip label_percentile_bars formatter
        |> shift_positive
        |> List.map ~f:(fun (l, s) -> (l, float s))
    in

    bars
    |> Text_graph.render ~narrow:true
    |> print_endline

  let readme () =
    String_extended.word_wrap ~soft_limit:75
      "Specify a single interest and the variable you would like plotted. \
       The VARIABLE must be one of time, value, delta or time_delta.\n\n\
       By default, a density plot is produced, with the 'variable' (time, value, ...) \
       as the independent variable, and frequency-of-occurance as the dependent. \
       Specifying the -percentiles flag causes the value (dependent variable) to be \
       plotted against the percentile (independent variable).\n\n\
       See the  help-interests  subcommand for information on specifying the INTEREST."

  let command =
    Command.basic_spec
      ~summary:"Plot density and percentile graphs"
      ~readme
      Command.Spec.(
        empty
        +> anon ("interest" %: Interest.arg_type)
        +> variable_arg
        +> plot_percentiles
        +> plot_max_bins
        +> plot_log_scale
        +> iobuf_file_flag ()
      )
      main
end

let interests_readme =
  Command.basic_spec
    ~summary:"Display the readme for INTERESTs"
    Command.Spec.empty
    (fun () ->
       Lazy.force Interest.readme
       |> String_extended.word_wrap ~soft_limit:75
       |> print_endline
    )

let command =
  Command.group
    ~summary:"Analyse a core-profiler file produced by Core_profiler.Offline."
    [ ("list", List_command.command)
    ; ("log", Log_command.command)
    ; ("summary", Summary_command.command)
    ; ("plot", Plot_command.command)
    ; ("help-interests", interests_readme)
    ]

let () =
  Command.run command
