open! Import
open! Async
module R = Render.Make (Draw_notty)

module Hierarchy = struct
  type node =
    { mutable visible : bool
    ; signals : Wave.t list
    ; children : node Base.Map.M(String).t
    }
  [@@deriving sexp_of]

  type currently_rendered =
    { actual_wave : Wave.t array
    ; for_rendering : Wave.t array
    }
  [@@deriving sexp_of]

  type t =
    { cfg : Waves.Config.t
    ; node : node
    ; mutable currently_rendered : currently_rendered option
    }
  [@@deriving sexp_of]

  let empty_node =
    { visible = false; signals = []; children = Map.empty (module String) }
  ;;

  let rec update ~path ~wave t =
    match path with
    | [] -> assert false
    | [ _ ] -> { t with signals = wave :: t.signals }
    | hd :: tl ->
      let children =
        Map.update t.children hd ~f:(function
          | None -> update ~path:tl ~wave empty_node
          | Some x -> update ~path:tl ~wave x)
      in
      { t with children }
  ;;

  let toggle_module t name =
    let rec loop path (node : node) =
      match path with
      | [] -> node.visible <- not node.visible
      | hd :: tl -> loop tl (Map.find_exn node.children hd)
    in
    (try loop (String.split name ~on:'$') t.node with
     | Not_found_s _ -> raise_s [%message "Cannot resolve key in module" name]);
    t.currently_rendered <- None
  ;;

  let of_waves (waves : Waves.t) =
    let init = empty_node in
    let ret =
      Array.fold waves.waves ~init ~f:(fun acc wave ->
        let path = String.split ~on:'$' (Wave.get_name wave) in
        update ~path ~wave acc)
    in
    ret.visible <- true;
    { cfg = waves.cfg; node = ret; currently_rendered = None }
  ;;

  let iter_wave ~f t =
    let rec loop ~depth ~rev_path node =
      if node.visible
      then (
        List.iter ~f:(fun node -> f ~depth node) node.signals;
        Map.iteri node.children ~f:(fun ~key ~data:node ->
          let module_name = String.concat ~sep:"$" (List.rev (key :: rev_path)) in
          f ~depth (Wave.Empty module_name);
          loop ~rev_path:(key :: rev_path) ~depth:(depth + 1) node))
    in
    loop ~depth:0 ~rev_path:[] t.node
  ;;

  let set_currently_rendered t =
    let actual_wave = ref [] in
    let for_rendering = ref [] in
    let () =
      iter_wave t ~f:(fun ~depth w ->
        actual_wave := w :: !actual_wave;
        let padding = String.init (depth * 2) ~f:(fun _ -> ' ') in
        let name = String.split ~on:'$' (Wave.get_name w) |> List.last_exn in
        let name =
          match w with
          | Empty _ -> "<" ^ name ^ ">"
          | _ -> name
        in
        for_rendering := Wave.set_name w (padding ^ name) :: !for_rendering)
    in
    let for_rendering = Array.of_list_rev !for_rendering in
    let actual_wave = Array.of_list_rev !actual_wave in
    t.currently_rendered <- Some { for_rendering; actual_wave }
  ;;

  let find_actual_wave t i =
    set_currently_rendered t;
    (Option.value_exn t.currently_rendered).actual_wave.(i)
  ;;

  let get_currently_rendered_waves t =
    set_currently_rendered t;
    let currently_rendered = (Option.value_exn t.currently_rendered).for_rendering in
    { Waves.cfg = t.cfg; waves = currently_rendered }
  ;;
end

module Signals_window = struct
  type t =
    { hierarchy : Hierarchy.t
    ; max_signal_name_width : int
    ; num_waves : int
    ; style : (Draw_notty.style[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create ~waves ~hierarchy =
    { hierarchy
    ; max_signal_name_width = R.get_max_signal_width waves
    ; num_waves = R.get_max_signals waves
    ; style = Render.Styles.(colour white_on_black).signals
    }
  ;;

  let draw ~ctx ~bounds t =
    R.draw_signals
      ~style:t.style
      ~ctx
      ~bounds
      (Hierarchy.get_currently_rendered_waves t.hierarchy)
  ;;
end

module Values_window = struct
  type t =
    { hierarchy : Hierarchy.t
    ; mutable max_value_width : int
    ; num_waves : int
    ; style : (Draw_notty.style[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create ~waves ~hierarchy =
    let max_value_width = R.get_estimated_max_value_width waves in
    { hierarchy
    ; max_value_width
    ; num_waves = R.get_max_signals waves
    ; style = Render.Styles.(colour white_on_black).values
    }
  ;;

  let draw ~ctx ~bounds t =
    let offset = t.hierarchy.cfg.value_scroll in
    t.hierarchy.cfg.value_scroll
    <- max 0 (min (t.max_value_width - 1) (t.max_value_width - offset));
    t.max_value_width
    <- R.draw_values
         ~style:t.style
         ~ctx
         ~bounds
         (Hierarchy.get_currently_rendered_waves t.hierarchy);
    t.hierarchy.cfg.value_scroll <- offset
  ;;
end

module Waves_window = struct
  type t =
    { hierarchy : Hierarchy.t
    ; mutable num_cycles : int
    ; num_waves : int
    ; style : (Draw_notty.style[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create ~(waves : Waves.t) ~hierarchy =
    { hierarchy
    ; num_cycles = 0
    ; num_waves = R.get_max_signals waves
    ; style = Render.Styles.(colour white_on_black).waves
    }
  ;;

  let draw ~ctx ~bounds t =
    R.draw_wave
      ~style:t.style
      ~ctx
      ~bounds
      (Hierarchy.get_currently_rendered_waves t.hierarchy)
  ;;
end

module With_bounds = struct
  type 'a t =
    { bounds : Draw.rect
    ; window : 'a
    }
  [@@deriving sexp_of]
end

module Border = struct
  let adjust (x : Draw.rect) =
    { Draw.r = x.r + 1; c = x.c + 1; w = x.w - 2; h = x.h - 2 }
  ;;

  let draw ~ctx ~bounds label =
    Draw_notty.draw_box ~ctx ~bounds ~style:Draw.Style.default label
  ;;
end

module Waveform_window = struct
  type t =
    { signals_window : Signals_window.t With_bounds.t
    ; values_window : Values_window.t With_bounds.t
    ; waves_window : Waves_window.t With_bounds.t
    ; scroll_signals : Scroll.HScrollbar.t
    ; scroll_values : Scroll.HScrollbar.t
    ; scroll_waves : Scroll.HScrollbar.t
    ; scroll_vert : Scroll.VScrollbar.t
    ; max_signal_offset : int
    ; max_cycle_offset : int
    }
  [@@deriving sexp_of]

  let get_signal_offset (t : t) = t.waves_window.window.hierarchy.cfg.start_signal

  let set_signal_offset (t : t) offset =
    t.waves_window.window.hierarchy.cfg.start_signal
    <- max 0 (min (t.max_signal_offset - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_vert.scrollable offset
  ;;

  let get_cycle_offset (t : t) = t.waves_window.window.hierarchy.cfg.start_cycle

  let set_cycle_offset (t : t) offset =
    t.waves_window.window.hierarchy.cfg.start_cycle
    <- max 0 (min (t.max_cycle_offset - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_waves.scrollable offset
  ;;

  let _get_signal_name_offset (t : t) =
    t.signals_window.window.hierarchy.cfg.signal_scroll
  ;;

  let set_signal_name_offset (t : t) offset =
    t.signals_window.window.hierarchy.cfg.signal_scroll
    <- max 0 (min (t.signals_window.window.max_signal_name_width - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_signals.scrollable offset
  ;;

  let _get_value_offset (t : t) = t.values_window.window.hierarchy.cfg.value_scroll

  let set_value_offset (t : t) offset =
    t.values_window.window.hierarchy.cfg.value_scroll
    <- max 0 (min (t.values_window.window.max_value_width - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_values.scrollable offset
  ;;

  let create ~signals_width ~values_width ~rows ~cols waves =
    let hbarheight = 1 in
    let vbarwidth = 2 in
    let hierarchy = Hierarchy.of_waves waves in
    let signals_window : Signals_window.t With_bounds.t =
      { bounds = { r = 0; c = 0; w = signals_width; h = rows - hbarheight }
      ; window = Signals_window.create ~waves ~hierarchy
      }
    in
    let values_window : Values_window.t With_bounds.t =
      { bounds = { r = 0; c = signals_width; w = values_width; h = rows - hbarheight }
      ; window = Values_window.create ~waves ~hierarchy
      }
    in
    let waves_window : Waves_window.t With_bounds.t =
      let sum = signals_width + values_width in
      { bounds = { r = 0; c = sum; w = cols - sum - vbarwidth; h = rows - hbarheight }
      ; window = Waves_window.create ~waves ~hierarchy
      }
    in
    let scroll_vert =
      Scroll.VScrollbar.create
        { Draw.r = 0; c = cols - vbarwidth; w = vbarwidth; h = rows - hbarheight }
    in
    let scroll_signals =
      Scroll.HScrollbar.create
        { Draw.r = rows - hbarheight; c = 0; w = signals_width; h = hbarheight }
    in
    let scroll_values =
      Scroll.HScrollbar.create
        { Draw.r = rows - hbarheight
        ; c = signals_width
        ; w = values_width
        ; h = hbarheight
        }
    in
    let scroll_waves =
      let sum = signals_width + values_width in
      Scroll.HScrollbar.create
        { Draw.r = rows - hbarheight
        ; c = sum
        ; w = cols - sum - vbarwidth
        ; h = hbarheight
        }
    in
    let max_signal_offset = R.get_max_signals waves in
    let max_cycle_offset = R.get_max_cycles waves in
    let waveform =
      { signals_window
      ; values_window
      ; waves_window
      ; scroll_signals
      ; scroll_values
      ; scroll_waves
      ; scroll_vert
      ; max_signal_offset
      ; max_cycle_offset
      }
    in
    Scroll.Scrollable.set_range scroll_vert.scrollable signals_window.window.num_waves;
    scroll_vert.scrollable.adj.on_offset_change <- set_signal_offset waveform;
    Scroll.Scrollable.set_range scroll_waves.scrollable max_cycle_offset;
    scroll_waves.scrollable.adj.on_offset_change <- set_cycle_offset waveform;
    Scroll.Scrollable.set_range
      scroll_signals.scrollable
      signals_window.window.max_signal_name_width;
    scroll_signals.scrollable.adj.on_offset_change <- set_signal_name_offset waveform;
    Scroll.Scrollable.set_range
      scroll_values.scrollable
      values_window.window.max_value_width;
    Scroll.Scrollable.set_offset
      scroll_values.scrollable
      (values_window.window.max_value_width - 1);
    scroll_values.scrollable.adj.on_offset_change <- set_value_offset waveform;
    waveform
  ;;

  let draw ~ctx (t : t) =
    let draw_with_border f ~ctx ~bounds name a =
      f ~ctx ~bounds:(Border.adjust bounds) a;
      Border.draw ~ctx ~bounds name
    in
    draw_with_border
      Signals_window.draw
      ~ctx
      ~bounds:t.signals_window.bounds
      "signals"
      t.signals_window.window;
    draw_with_border
      Values_window.draw
      ~ctx
      ~bounds:t.values_window.bounds
      "values"
      t.values_window.window;
    draw_with_border
      Waves_window.draw
      ~ctx
      ~bounds:t.waves_window.bounds
      "waves"
      t.waves_window.window;
    Scroll.VScrollbar.draw ~ctx ~style:Draw.Style.default t.scroll_vert;
    Scroll.HScrollbar.draw ~ctx ~style:Draw.Style.default t.scroll_signals;
    Scroll.HScrollbar.draw ~ctx ~style:Draw.Style.default t.scroll_values;
    Scroll.HScrollbar.draw ~ctx ~style:Draw.Style.default t.scroll_waves
  ;;

  let scale_key_handler (t : t) key =
    let cfg = t.waves_window.window.hierarchy.cfg in
    match key with
    | `ASCII '=', [] ->
      cfg.wave_width <- cfg.wave_width + 1;
      true
    | `ASCII '-', [] ->
      cfg.wave_width <- cfg.wave_width - 1;
      true
    | `ASCII '+', [] ->
      cfg.wave_height <- cfg.wave_height + 1;
      true
    | `ASCII '_', [] ->
      cfg.wave_height <- max 0 (cfg.wave_height - 1);
      true
    | _ -> false
  ;;

  let scroll_key_handler (t : t) key =
    match key with
    | `Arrow `Left, [] ->
      set_cycle_offset t (get_cycle_offset t - 1);
      true
    | `Arrow `Right, [] ->
      set_cycle_offset t (get_cycle_offset t + 1);
      true
    | `Arrow `Up, [] ->
      set_signal_offset t (get_signal_offset t - 1);
      true
    | `Arrow `Down, [] ->
      set_signal_offset t (get_signal_offset t + 1);
      true
    | _ -> false
  ;;

  let zrect = { Draw.r = 0; c = 0; w = 0; h = 0 }

  let last_mouse_button : (Notty.Unescape.button * Notty.Unescape.mods) option ref =
    ref None
  ;;

  let mouse_handler (t : t) ((button, (col, row), mods) as mouse : Notty.Unescape.mouse) =
    let cfg = t.waves_window.window.hierarchy.cfg in
    let pick f =
      match
        R.pick
          ~bounds:
            { waves = t.waves_window.bounds
            ; values = zrect
            ; signals = zrect
            ; status = zrect
            }
          ~r:row
          ~c:col
          (Hierarchy.get_currently_rendered_waves t.waves_window.window.hierarchy)
      with
      | R.Wave (cycle, signal) ->
        f cycle signal;
        true
      | _ -> false
    in
    let in_bounds (bounds : Draw.rect) =
      row >= bounds.r
      && col >= bounds.c
      && row < bounds.r + bounds.h
      && col < bounds.c + bounds.w
    in
    let toggle_module button =
      if in_bounds t.signals_window.bounds && Poly.equal button (Some (`Left, []))
      then (
        let offset = get_signal_offset t in
        let hierarchy = t.signals_window.window.hierarchy in
        let wave_height = hierarchy.cfg.wave_height in
        let waves = Hierarchy.get_currently_rendered_waves hierarchy in
        let selected_index =
          let current_position = ref (row - 1) in
          let rec loop i lower =
            if i = Array.length waves.waves
            then None
            else (
              let delta = snd (R.get_wave_height (wave_height, waves.waves.(i))) in
              if i < offset
              then (
                current_position := !current_position + delta;
                loop (i + 1) (lower + delta))
              else if lower <= !current_position && !current_position < lower + delta
              then Some i
              else loop (i + 1) (lower + delta))
          in
          loop 0 0
        in
        match selected_index with
        | None -> false
        | Some selected_index ->
          (match waves.waves.(selected_index) with
           | Empty _ ->
             let name =
               Wave.get_name (Hierarchy.find_actual_wave hierarchy selected_index)
             in
             Hierarchy.toggle_module hierarchy name;
             true
           | _ -> false))
      else false
    in
    let update_cursor button =
      if in_bounds t.waves_window.bounds && Poly.equal button (Some (`Left, []))
      then pick (fun cycle _ -> cfg.wave_cursor <- cycle)
      else false
    in
    let update_mouse_button_scroll button =
      match button with
      | Some (`Scroll `Up, []) ->
        set_signal_offset t (get_signal_offset t - 1);
        true
      | Some (`Scroll `Down, []) ->
        set_signal_offset t (get_signal_offset t + 1);
        true
      | Some (`Scroll `Up, [ `Ctrl ]) ->
        set_cycle_offset t (get_cycle_offset t - 1);
        true
      | Some (`Scroll `Down, [ `Ctrl ]) ->
        set_cycle_offset t (get_cycle_offset t + 1);
        true
      | _ -> false
    in
    let update_scroll_bar (scroll : Scroll.Scrollbar.t) _ =
      in_bounds scroll.bounds && Scroll.Scrollbar.mouse_event scroll mouse
    in
    match button with
    | `Press b ->
      last_mouse_button := Some (b, mods);
      List.fold_left
        [ update_cursor
        ; update_mouse_button_scroll
        ; update_scroll_bar t.scroll_vert
        ; update_scroll_bar t.scroll_waves
        ; update_scroll_bar t.scroll_signals
        ; update_scroll_bar t.scroll_values
        ; toggle_module
        ]
        ~init:false
        ~f:(fun acc f -> acc || f !last_mouse_button)
    | `Release ->
      let button = !last_mouse_button in
      last_mouse_button := None;
      update_cursor button
    | `Drag -> update_cursor !last_mouse_button
  ;;

  (* return true to redraw *)
  let handler (t : t) event =
    match event with
    | `Mouse mouse -> mouse_handler t mouse
    | `Key key ->
      List.fold_left
        [ scale_key_handler; scroll_key_handler ]
        ~init:false
        ~f:(fun acc f -> acc || f t key)
    | `Resize _ | `Paste _ -> false
  ;;
end

module Context = struct
  type t =
    { term : Notty_async.Term.t
    ; mutable rows : int
    ; mutable cols : int
    ; waves : Waves.t
    ; mutable waveform : Waveform_window.t
    ; events : [ Notty.Unescape.event | `Resize of int * int ] Pipe.Reader.t
    ; stop : unit Deferred.t
    ; mutable draw_ctx : Draw_notty.ctx
    ; signals_width : int
    ; values_width : int
    }

  let create ~signals_width ~values_width waves =
    let%bind term = Notty_async.Term.create () in
    let cols, rows = Notty_async.Term.size term in
    let waveform =
      Waveform_window.create ~signals_width ~values_width ~cols ~rows waves
    in
    let events = Notty_async.Term.events term in
    let stop = Pipe.closed events in
    let%bind () = Notty_async.Term.cursor term None in
    let draw_ctx = Draw_notty.init ~rows ~cols in
    return
      { term
      ; rows
      ; cols
      ; events
      ; stop
      ; waves
      ; waveform
      ; draw_ctx
      ; signals_width
      ; values_width
      }
  ;;

  let resize ~rows ~cols t =
    let waveform =
      Waveform_window.create
        ~signals_width:t.signals_width
        ~values_width:t.values_width
        ~cols
        ~rows
        t.waves
    in
    let draw_ctx = Draw_notty.init ~rows ~cols in
    t.rows <- rows;
    t.cols <- cols;
    t.waveform <- waveform;
    t.draw_ctx <- draw_ctx
  ;;

  let draw (ctx : t) =
    Waveform_window.draw ~ctx:ctx.draw_ctx ctx.waveform;
    let image = Draw_notty.to_image ctx.draw_ctx in
    Notty_async.Term.image ctx.term image
  ;;

  let handle_event ctx event =
    let handler event = Waveform_window.handler ctx.waveform event in
    match event with
    | `Mouse _ -> handler event
    | `Key key ->
      (match key with
       | `ASCII 'q', [] | `Escape, [] ->
         Pipe.close_read ctx.events;
         false
       | _ -> handler event)
    | `Resize (cols, rows) ->
      resize ~rows ~cols ctx;
      true
    | `Paste _ -> false
  ;;

  let iter_events ctx =
    (* process events in batches and draw at the end. Given rendering can be slow, this
       behaves much better - especially over a ssh connection. *)
    Pipe.iter' ctx.events ~f:(fun q ->
      let redraw =
        Core_kernel.Queue.fold q ~init:false ~f:(fun redraw event ->
          if handle_event ctx event then true else redraw)
      in
      if redraw then draw ctx else return ())
  ;;
end

let run_waves ?(signals_width = 20) ?(values_width = 20) waves =
  let%bind ctx = Context.create ~signals_width ~values_width waves in
  let%bind () = Context.draw ctx in
  don't_wait_for (Context.iter_events ctx);
  ctx.stop
;;

let run ?signals_width ?values_width waves =
  Thread_safe.block_on_async (fun () -> run_waves ?signals_width ?values_width waves)
  |> Result.ok_exn
;;

let run_and_close ?signals_width ?values_width waves =
  don't_wait_for
    (let%bind () = run_waves ?signals_width ?values_width waves in
     shutdown 0;
     return ());
  Core.never_returns (Scheduler.go ())
;;

let run_interactive_viewer ?signals_width ?values_width ?display_rules t =
  run_and_close
    ?signals_width
    ?values_width
    { cfg = Waves.Config.default
    ; waves = Waveform.sort_ports_and_formats t display_rules
    }
;;
