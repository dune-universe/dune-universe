open! Import
open! Async
module R = Render.Make (Draw_notty)

module Signals_window = struct
  type t =
    { waves : Waves.t
    ; max_signal_name_width : int
    ; num_waves : int
    ; style : Draw_notty.style sexp_opaque
    }
  [@@deriving sexp_of]

  let create (waves : Waves.t) =
    { waves
    ; max_signal_name_width = R.get_max_signal_width waves
    ; num_waves = R.get_max_signals waves
    ; style = Render.Styles.(colour white_on_black).signals
    }
  ;;

  let draw ~ctx ~bounds t = R.draw_signals ~style:t.style ~ctx ~bounds t.waves
end

module Values_window = struct
  type t =
    { waves : Waves.t
    ; mutable max_value_width : int
    ; num_waves : int
    ; style : Draw_notty.style sexp_opaque
    }
  [@@deriving sexp_of]

  let create (waves : Waves.t) =
    let max_value_width = R.get_estimated_max_value_width waves in
    { waves
    ; max_value_width
    ; num_waves = R.get_max_signals waves
    ; style = Render.Styles.(colour white_on_black).values
    }
  ;;

  let draw ~ctx ~bounds t =
    let offset = t.waves.cfg.value_scroll in
    (t.waves.cfg).value_scroll
    <- max 0 (min (t.max_value_width - 1) (t.max_value_width - offset));
    t.max_value_width <- R.draw_values ~style:t.style ~ctx ~bounds t.waves;
    (t.waves.cfg).value_scroll <- offset
  ;;
end

module Waves_window = struct
  type t =
    { waves : Waves.t
    ; mutable num_cycles : int
    ; num_waves : int
    ; style : Draw_notty.style sexp_opaque
    }
  [@@deriving sexp_of]

  let create (waves : Waves.t) =
    { waves
    ; num_cycles = 0
    ; num_waves = R.get_max_signals waves
    ; style = Render.Styles.(colour white_on_black).waves
    }
  ;;

  let draw ~ctx ~bounds t = R.draw_wave ~style:t.style ~ctx ~bounds t.waves
end

module With_bounds = struct
  type 'a t =
    { bounds : Draw.rect
    ; window : 'a
    }
  [@@deriving sexp_of]
end

module Border = struct
  let adjust (x : Draw.rect) = { Draw.r = x.r + 1; c = x.c + 1; w = x.w - 2; h = x.h - 2 }

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

  let get_signal_offset (t : t) = t.waves_window.window.waves.cfg.start_signal

  let set_signal_offset (t : t) offset =
    (t.waves_window.window.waves.cfg).start_signal
    <- max 0 (min (t.max_signal_offset - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_vert.scrollable offset
  ;;

  let get_cycle_offset (t : t) = t.waves_window.window.waves.cfg.start_cycle

  let set_cycle_offset (t : t) offset =
    (t.waves_window.window.waves.cfg).start_cycle
    <- max 0 (min (t.max_cycle_offset - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_waves.scrollable offset
  ;;

  let _get_signal_name_offset (t : t) = t.signals_window.window.waves.cfg.signal_scroll

  let set_signal_name_offset (t : t) offset =
    (t.signals_window.window.waves.cfg).signal_scroll
    <- max 0 (min (t.signals_window.window.max_signal_name_width - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_signals.scrollable offset
  ;;

  let _get_value_offset (t : t) = t.values_window.window.waves.cfg.value_scroll

  let set_value_offset (t : t) offset =
    (t.values_window.window.waves.cfg).value_scroll
    <- max 0 (min (t.values_window.window.max_value_width - 1) offset);
    Scroll.Scrollable.set_offset t.scroll_values.scrollable offset
  ;;

  let create ~signals_width ~values_width ~rows ~cols waves =
    let hbarheight = 1 in
    let vbarwidth = 2 in
    let signals_window : Signals_window.t With_bounds.t =
      { bounds = { r = 0; c = 0; w = signals_width; h = rows - hbarheight }
      ; window = Signals_window.create waves
      }
    in
    let values_window : Values_window.t With_bounds.t =
      { bounds = { r = 0; c = signals_width; w = values_width; h = rows - hbarheight }
      ; window = Values_window.create waves
      }
    in
    let waves_window : Waves_window.t With_bounds.t =
      let sum = signals_width + values_width in
      { bounds = { r = 0; c = sum; w = cols - sum - vbarwidth; h = rows - hbarheight }
      ; window = Waves_window.create waves
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
        { Draw.r = rows - hbarheight; c = signals_width; w = values_width; h = hbarheight
        }
    in
    let scroll_waves =
      let sum = signals_width + values_width in
      Scroll.HScrollbar.create
        { Draw.r = rows - hbarheight; c = sum; w = cols - sum - vbarwidth; h = hbarheight
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
    (scroll_vert.scrollable.adj).on_offset_change <- set_signal_offset waveform;
    Scroll.Scrollable.set_range scroll_waves.scrollable max_cycle_offset;
    (scroll_waves.scrollable.adj).on_offset_change <- set_cycle_offset waveform;
    Scroll.Scrollable.set_range
      scroll_signals.scrollable
      signals_window.window.max_signal_name_width;
    (scroll_signals.scrollable.adj).on_offset_change <- set_signal_name_offset waveform;
    Scroll.Scrollable.set_range
      scroll_values.scrollable
      values_window.window.max_value_width;
    Scroll.Scrollable.set_offset
      scroll_values.scrollable
      (values_window.window.max_value_width - 1);
    (scroll_values.scrollable.adj).on_offset_change <- set_value_offset waveform;
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
    let cfg = t.waves_window.window.waves.cfg in
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
    let cfg = t.waves_window.window.waves.cfg in
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
          t.waves_window.window.waves
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
    ; events : [Notty.Unescape.event | `Resize of int * int] Pipe.Reader.t
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
       | `ASCII 'q', []
       | `Escape, [] ->
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
    { cfg = Waves.Config.default; waves = Waveform.sort_ports_and_formats t display_rules
    }
;;
