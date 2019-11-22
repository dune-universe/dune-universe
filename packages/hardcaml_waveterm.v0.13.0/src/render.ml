open! Import

module Styles = struct
  type t =
    { border : Draw.Style.t option
    ; signals : Draw.Style.t
    ; values : Draw.Style.t
    ; waves : Draw.Style.t
    ; status : Draw.Style.t
    }

  let default d = { border = Some d; signals = d; values = d; waves = d; status = d }
  let white_on_black = default Draw.Style.{ default with fg = White; bg = Black }
  let black_on_white = default Draw.Style.{ default with fg = Black; bg = White }

  let colour s =
    { s with
      signals = Draw.Style.{ s.signals with fg = Blue }
    ; values = Draw.Style.{ s.values with fg = Red }
    ; waves = Draw.Style.{ s.waves with fg = Green }
    ; status = Draw.Style.{ s.waves with fg = Magenta }
    }
  ;;

  let colour_on_black = colour white_on_black
  let colour_on_white = colour black_on_white
end

module Bounds = struct
  (* XXX Some stuff would be much simplified if the rects excluded the border
     and we just adjusted for it here.  If the border is drawn it is on
     the outside of the rendering window *)

  type t =
    { signals : Draw.rect
    ; values : Draw.rect
    ; waves : Draw.rect
    ; status : Draw.rect
    }

  let expand_for_border x =
    let open Draw in
    if x.w <> 0 && x.h <> 0
    then Draw.{ r = x.r - 1; c = x.c - 1; w = x.w + 2; h = x.h + 2 }
    else x
  ;;

  let shrink_for_border x =
    let open Draw in
    if x.w <> 0 && x.h <> 0
    then Draw.{ r = x.r + 1; c = x.c + 1; w = max 0 (x.w - 2); h = max 0 (x.h - 2) }
    else x
  ;;

  let fit_to_window
        ?(signals = true)
        ?(values = true)
        ?(waves = true)
        ?(status = false)
        ?(border = true)
        bounds
    =
    let open Draw in
    let rows, cols = bounds.h, bounds.w in
    let minb = if border then 3 else 1 in
    let iw6 = max minb (min 20 (cols / 6)) in
    (* approx 1/6 of width, >minb and < 20 *)
    let iw4 = max minb (min 20 (cols / 4)) in
    (* approx 1/4 of width, >minb and < 20 *)
    let z = { r = 0; c = 0; w = 0; h = (rows - if status then 3 else 0) } in
    let get_bounds w0 w1 w2 =
      if w2 <= 0 && waves
      then failwith "windows wont fit (sorry, should be more graceful!)"
      else (
        let border x =
          if border && x.w <> 0 && x.h <> 0 then shrink_for_border x else x
        in
        { signals = border { z with w = w0 }
        ; values = border { z with c = w0; w = w1 }
        ; waves = border { z with c = w0 + w1; w = w2 }
        ; status =
            (if status
             then border { r = bounds.h - 3; c = 0; h = 3; w = bounds.w }
             else z)
        })
    in
    match signals, values, waves with
    (* all *)
    | true, true, true -> get_bounds iw6 iw6 (cols - iw6 - iw6)
    (* 2 *)
    | true, true, false -> get_bounds (cols / 2) ((cols + 1) / 2) 0
    | true, false, true -> get_bounds iw4 0 (cols - iw4)
    | false, true, true -> get_bounds 0 iw4 (cols - iw4)
    (* 1 *)
    | true, false, false -> get_bounds cols 0 0
    | false, true, false -> get_bounds 0 cols 0
    | false, false, true -> get_bounds 0 0 cols
    (* 0 *)
    | false, false, false -> get_bounds 0 0 0
  ;;
end

module Make (G : Draw.S) = struct
  open G
  open Wave

  let get_wave_width (w, d) =
    if w < 0
    then (
      (* subcycle rendering *)
      match d with
      | Empty _ | Clock _ -> w, 1
      | Binary _ | Data _ -> w, 1)
    else (
      match d with
      | Empty _ | Clock _ -> w, (w + 1) * 2
      | Data _ | Binary _ -> (w * 2) + 1, (w + 1) * 2)
  ;;

  let get_wave_height = function
    | 0, Empty _ | 0, Clock _ -> 0, 2
    | 0, Data _ -> 0, 2
    | 0, Binary _ -> 0, 2
    | 1, Empty _ | 1, Clock _ -> 0, 2
    | 1, Data _ -> 1, 3
    | 1, Binary _ -> 0, 2
    | h, Empty _ | h, Clock _ -> h - 1, h + 1
    | h, Data _ -> h - 1, h + 1
    | h, Binary _ -> h - 1, h + 1
  ;;

  let get_max_signal_width (state : Waves.t) =
    Array.fold state.waves ~init:0 ~f:(fun m s -> max m (String.length (get_name s)))
  ;;

  let get_max_value_width (state : Waves.t) =
    let fold f a d =
      let len = Data.length d in
      let rec g a i = if i = len then a else g (f a (Data.get d i)) (i + 1) in
      g a 0
    in
    Array.fold state.waves ~init:0 ~f:(fun m w ->
      try
        let data = Wave.get_data w in
        let to_str = Wave.get_to_str w in
        let max m s = max m (String.length (to_str s)) in
        fold max m data
      with
      | _ -> m)
  ;;

  let get_estimated_max_value_width (state : Waves.t) =
    let unsigned_width =
      let table =
        Array.init 64 ~f:(fun i ->
          if i = 0
          then 1
          else Bits.ones i |> Bits.to_int64 |> Int64.to_string |> String.length)
      in
      fun width -> table.(min 63 width)
    in
    let signed_width =
      let table =
        Array.init 65 ~f:(fun i ->
          if i = 0
          then 1
          else
            Bits.one i
            |> Bits.reverse
            |> Bits.to_int64
            |> Int64.to_string
            |> String.length)
      in
      fun width -> table.(min 64 width)
    in
    Array.fold state.waves ~init:0 ~f:(fun max_width wave ->
      let bits =
        try Bits.width (Data.get (Wave.get_data wave) 0) with
        | _ -> 0
      in
      let rec get_width fmt =
        match (fmt : Wave_format.t) with
        | Binary -> bits
        | Bit -> 1
        | Bit_or t -> if bits = 0 then 1 else get_width t
        | Hex -> (bits + 3) / 4
        | Unsigned_int -> unsigned_width bits
        | Int -> signed_width bits
        | Custom _ -> 8 (* could add a width hint *)
        | Index s ->
          List.fold_left s ~init:0 ~f:(fun mx str -> max mx (String.length str))
      in
      max max_width (get_width (Wave.get_format wave)))
  ;;

  let get_max_cycles (state : Waves.t) =
    Array.fold state.waves ~init:0 ~f:(fun m d ->
      max
        m
        (try Data.length (Wave.get_data d) with
         | _ -> 0))
  ;;

  let get_max_signals (state : Waves.t) = Array.length state.waves
  let get_w_scale w = if w < -1 then -w else 1

  let get_max_wave_width (state : Waves.t) =
    let cycles = get_max_cycles state in
    let w, waw = get_wave_width (state.cfg.wave_width, Clock "") in
    let w_scale = get_w_scale w in
    waw * ((cycles + w_scale - 1) / w_scale)
  ;;

  let get_max_wave_height (state : Waves.t) start_signal =
    let rec f acc i =
      if i < Array.length state.waves
      then (
        let _, wah = get_wave_height (state.cfg.wave_height, state.waves.(i)) in
        f (acc + wah) (i + 1))
      else acc
    in
    f 0 start_signal
  ;;

  let get_max_bounds state =
    let open Draw in
    let swidth = get_max_signal_width state in
    let vwidth = get_max_value_width state in
    let wwidth = get_max_wave_width state in
    let wheight = get_max_wave_height state state.cfg.start_signal in
    let z = { r = 0; c = 0; h = wheight; w = 0 } in
    let open Bounds in
    { signals = { z with w = swidth }
    ; values = { z with w = vwidth }
    ; waves = { z with w = wwidth }
    ; status = z
    }
  ;;

  let draw_clock_cycle ~ctx ~style ~bounds ~w ~h ~c =
    let open Draw in
    if w < 0
    then (
      for c = c to c + 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c BH
      done;
      for r = 1 to h do
        for c = c to c + 1 do
          draw_piece ~ctx ~style ~bounds ~r ~c F
        done
      done;
      for c = c to c + 1 do
        draw_piece ~ctx ~style ~bounds ~r:(h + 1) ~c TH
      done)
    else (
      draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
      for i = 0 to w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + 1 + i) H
      done;
      draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + w + 1) BL;
      for i = 0 to h - 1 do
        draw_piece ~ctx ~style ~bounds ~r:(0 + i + 1) ~c:(c + w + 1) V
      done;
      draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c:(c + w + 1) TR;
      for i = 0 to w - 1 do
        draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c:(c + w + 2 + i) H
      done;
      draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c:(c + w + w + 2) TL;
      for i = 0 to h - 1 do
        draw_piece ~ctx ~style ~bounds ~r:(0 + i + 1) ~c:(c + w + w + 2) V
      done)
  ;;

  let draw_clock_cycles ~ctx ~style ~bounds ~w ~waw ~h ~cnt =
    for i = 0 to cnt - 1 do
      draw_clock_cycle ~ctx ~style ~bounds ~w ~h ~c:(i * waw)
    done
  ;;

  let wget data i =
    try Data.get data i with
    | _ -> Data.get data (Data.length data - 1)
  ;;

  let get_fuzzy_data data i w_scale =
    let rec f i w_scale prev =
      if w_scale = 0
      then Some prev
      else (
        let d = wget data i in
        if Bits.equal d prev then f (i + 1) (w_scale - 1) prev else None)
    in
    let d = wget data i in
    (* if we get 1 element, then we succeed *)
    try f (i + 1) (w_scale - 1) d with
    | _ -> Some d
  ;;

  let get_data_index off i w_scale =
    if w_scale < -1
    then (
      let w_scale = get_w_scale w_scale in
      (w_scale * i) + off)
    else off + i
  ;;

  let get_data data off i w_scale =
    if w_scale < -1
    then (
      let w_scale = get_w_scale w_scale in
      get_fuzzy_data data ((w_scale * i) + off) w_scale)
    else Some (wget data (off + i))
  ;;

  let draw_binary_data ~ctx ~style ~bounds ~w ~h ~data ~off =
    let open Draw in
    let w_scale, w = w, max 0 w in
    let rec f prev c i =
      if c >= bounds.w || get_data_index off i w_scale >= Data.length data
      then ()
      else (
        let cur = get_data data off i w_scale in
        let low () =
          for i = 0 to w do
            draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c:(c + i) H
          done
        in
        let low_high () =
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BR;
          for i = 0 + 1 to 0 + h + 1 do
            draw_piece ~ctx ~style ~bounds ~r:i ~c V
          done;
          draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c TL;
          for i = 1 to w do
            draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + i) H
          done
        in
        let high_low () =
          draw_piece ~ctx ~style ~bounds ~r:0 ~c BL;
          for i = 0 + 1 to 0 + h + 1 do
            draw_piece ~ctx ~style ~bounds ~r:i ~c V
          done;
          draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c TR;
          for i = 1 to w do
            draw_piece ~ctx ~style ~bounds ~r:(0 + h + 1) ~c:(c + i) H
          done
        in
        let high () =
          for i = 0 to w do
            draw_piece ~ctx ~style ~bounds ~r:0 ~c:(c + i) H
          done
        in
        let fuzz () =
          for c = c to c + w do
            draw_piece ~ctx ~style ~bounds ~r:0 ~c BH
          done;
          for c = c to c + w do
            for r = 1 to h do
              draw_piece ~ctx ~style ~bounds ~r ~c F
            done
          done;
          for c = c to c + w do
            draw_piece ~ctx ~style ~bounds ~r:(h + 1) ~c TH
          done
        in
        let fuzzy p = Option.is_none p in
        let zero = function
          | Some p -> Bits.is_gnd p
          | _ -> false
        in
        let one = function
          | Some p -> Bits.is_vdd p
          | _ -> false
        in
        if fuzzy cur
        then fuzz ()
        else if fuzzy prev && zero cur
        then low ()
        else if fuzzy prev && one cur
        then high ()
        else if zero prev && zero cur
        then low ()
        else if one prev && zero cur
        then high_low ()
        else if zero prev && one cur
        then low_high ()
        else if one prev && one cur
        then high ()
        else failwith "not binary data";
        f cur (c + w + 1) (i + 1))
    in
    try f None 0 0 with
    | _ -> ()
  ;;

  let draw_data ~ctx ~style ~bounds ~to_str ~alignment ~w ~h ~data ~off =
    let w_scale, w = w, max 0 w in
    let draw_text r c cnt data =
      match data with
      | None -> ()
      | Some data ->
        let str = to_str data in
        let putc i ch = draw_char ~ctx ~style ~bounds ~r ~c:(c + i) ch in
        let str_len = String.length str in
        if str_len <= cnt
        then
          for i = 0 to str_len - 1 do
            putc i str.[i]
          done
        else (
          match alignment with
          | Wave_format.Left ->
            for i = 0 to cnt - 1 do
              putc i (if i = cnt - 1 then '.' else str.[i])
            done
          | Right ->
            for i = 0 to cnt - 1 do
              putc i (if i = 0 then '.' else str.[str_len - 1 - (cnt - 1 - i)])
            done)
    in
    let rec f prev prev_cnt c i =
      let open Draw in
      let r = 0 in
      if c >= bounds.w || get_data_index off i w_scale >= Data.length data
      then (if h > 0 then draw_text (r + 1 + ((h - 1) / 2)) (c - prev_cnt) prev_cnt prev)
      else (
        let cur = get_data data off i w_scale in
        let fuzzy p = Option.is_none p in
        let same a b =
          match a, b with
          | Some a, Some b when Bits.equal a b -> true
          | _ -> false
        in
        let transn () =
          draw_piece ~ctx ~style ~bounds ~r ~c T;
          for r = r + 1 to r + h do
            draw_piece ~ctx ~style ~bounds ~r ~c V
          done;
          draw_piece ~ctx ~style ~bounds ~r:(r + h + 1) ~c Tu;
          for c = c + 1 to c + w do
            draw_piece ~ctx ~style ~bounds ~r ~c H;
            draw_piece ~ctx ~style ~bounds ~r:(r + h + 1) ~c H
          done
        in
        let extend () =
          for c = c to c + w do
            draw_piece ~ctx ~style ~bounds ~r ~c H;
            draw_piece ~ctx ~style ~bounds ~r:(r + h + 1) ~c H
          done
        in
        let fuzz () =
          for c = c to c + w do
            draw_piece ~ctx ~style ~bounds ~r:0 ~c BH
          done;
          for c = c to c + w do
            for r = 1 to h do
              draw_piece ~ctx ~style ~bounds ~r ~c F
            done
          done;
          for c = c to c + w do
            draw_piece ~ctx ~style ~bounds ~r:(h + 1) ~c TH
          done
        in
        let run fn txt ext =
          fn ();
          if txt && h > 0
          then draw_text (r + 1 + ((h - 1) / 2)) (c - prev_cnt) prev_cnt prev;
          f cur (if ext then prev_cnt + w + 1 else w) (c + w + 1) (i + 1)
        in
        if fuzzy cur && not (fuzzy prev)
        then run fuzz true false
        else if fuzzy cur && fuzzy prev
        then run fuzz false false
        else if fuzzy prev
        then run extend false false
        else if same prev cur
        then run extend false true
        else run transn true false)
    in
    (*try f None (-1) 0 0
      with _ -> ()*)
    f None (-1) 0 0
  ;;

  let rec draw_iter i bounds (state : Waves.t) f =
    let open Draw in
    if i < Array.length state.waves && bounds.h > 0
    then (
      let _, wah = get_wave_height (state.cfg.wave_height, state.waves.(i)) in
      f i bounds state.waves.(i);
      draw_iter (i + 1) { bounds with r = bounds.r + wah; h = bounds.h - wah } state f)
  ;;

  type 'a draw_item =
    ?style:Draw.Style.t -> ctx:G.ctx -> bounds:Draw.rect -> Waves.t -> 'a

  let with_border
    : draw:'a draw_item -> label:string -> ?border:Draw.Style.t -> 'a draw_item
    =
    fun ~(draw : 'a draw_item)
      ~label
      ?border
      ?(style = Draw.Style.default)
      ~ctx
      ~bounds
      state ->
      let r = draw ~style ~ctx ~bounds state in
      match border with
      | Some border when bounds.Draw.w > 0 && bounds.Draw.h > 0 ->
        G.draw_box
          ~ctx
          ~style:(get_style border)
          ~bounds:(Bounds.expand_for_border bounds)
          label;
        r
      | _ -> r
  ;;

  let draw_cursor ~ctx ~bounds ~(state : Waves.t) =
    let open Draw in
    let w, waw = get_wave_width (state.cfg.wave_width, Clock "") in
    let w_scale = get_w_scale w in
    let cycle = state.cfg.wave_cursor - state.cfg.start_cycle in
    let c = cycle * waw / w_scale in
    for r = 0 to bounds.h - 1 do
      (* assume clipped when drawn *)
      inv ~ctx ~bounds ~r ~c
    done
  ;;

  let draw_wave ?(style = Draw.Style.default) ~ctx ~bounds (state : Waves.t) =
    let open Draw in
    let style = get_style style in
    (*let max_cycles = get_max_cycles state in*)
    fill ~ctx ~bounds ~style ' ';
    draw_iter state.cfg.start_signal bounds state (fun _ bounds wave ->
      let wh, _ = get_wave_height (state.cfg.wave_height, wave) in
      let ww, waw = get_wave_width (state.cfg.wave_width, wave) in
      let cnt = (bounds.w + waw - 1) / waw in
      let off = state.cfg.start_cycle in
      (*let cnt = max 0 ((min (off+cnt) max_cycles) - off) in*)
      match wave with
      | Empty _ -> ()
      | Clock _ -> draw_clock_cycles ~ctx ~style ~bounds ~w:ww ~waw ~h:wh ~cnt
      | Binary (_, data) ->
        let off = min (Data.length data - 1) off in
        draw_binary_data ~ctx ~style ~bounds ~w:ww ~h:wh ~data ~off
      | Data (_, data, _, alignment) ->
        let off = min (Data.length data - 1) off in
        draw_data
          ~ctx
          ~style
          ~bounds
          ~alignment
          ~to_str:(Wave.get_to_str wave)
          ~w:ww
          ~h:wh
          ~data
          ~off);
    draw_cursor ~ctx ~bounds ~state
  ;;

  let draw_highlight ~ctx ~bounds ~r b =
    if b
    then
      for c = 0 to bounds.Draw.w - 1 do
        inv ~ctx ~bounds ~r ~c
      done
  ;;

  let ssub s a b = String.sub s ~pos:a ~len:b

  let draw_scroll_string ~ctx ~style ~bounds ~r ~c str =
    let len = String.length str in
    let w = bounds.Draw.w in
    if len <= w
    then draw_string ~ctx ~style ~bounds ~r ~c:0 str
    else (
      let c = min c (len - w) in
      let str =
        try ssub str c w with
        | _ -> ""
      in
      draw_string ~ctx ~style ~bounds ~r ~c:0 str)
  ;;

  let draw_scroll_string_right ~ctx ~style ~bounds ~r ~c str =
    let len = String.length str in
    let w = bounds.Draw.w in
    let sub_right s o l =
      try ssub s (len - l - o) l with
      | _ -> ""
    in
    let draw_string_right ~ctx ~style ~bounds ~r str =
      let c = w - String.length str in
      draw_string ~ctx ~style ~bounds ~r ~c str
    in
    if len <= w
    then draw_string_right ~ctx ~style ~bounds ~r str
    else (
      let c = min c (len - w) in
      draw_string_right ~ctx ~style ~bounds ~r (sub_right str c w))
  ;;

  let draw_signals ?(style = Draw.Style.default) ~ctx ~bounds (state : Waves.t) =
    let style = get_style style in
    fill ~ctx ~bounds ~style ' ';
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let _, wah = get_wave_height (state.cfg.wave_height, wave) in
      let r = (wah - 1) / 2 in
      draw_scroll_string
        ~ctx
        ~style
        ~bounds
        ~r
        ~c:state.cfg.signal_scroll
        (Wave.get_name wave);
      draw_highlight ~ctx ~bounds ~r (i = state.cfg.signal_cursor))
  ;;

  let draw_values ?(style = Draw.Style.default) ~ctx ~bounds (state : Waves.t) =
    let style = get_style style in
    fill ~ctx ~bounds ~style ' ';
    let off =
      if state.cfg.wave_cursor < 0 then state.cfg.start_cycle else state.cfg.wave_cursor
    in
    let max_string_length = ref 0 in
    draw_iter state.cfg.start_signal bounds state (fun i bounds wave ->
      let _, wah = get_wave_height (state.cfg.wave_height, wave) in
      let r = (wah - 1) / 2 in
      (match wave with
       | Empty _ | Clock _ -> ()
       | Binary (_, d) ->
         let d =
           try Data.get d off with
           | _ -> Data.get d (Data.length d - 1)
         in
         let str = Bits.to_bstr d in
         max_string_length := max !max_string_length (String.length str);
         draw_scroll_string_right ~ctx ~style ~bounds ~r ~c:state.cfg.value_scroll str
       | Data (_, d, _, _alignment) ->
         let d =
           try Data.get d off with
           | _ -> Data.get d (Data.length d - 1)
         in
         let to_str = Wave.get_to_str wave in
         let str = to_str d in
         max_string_length := max !max_string_length (String.length str);
         draw_scroll_string_right ~ctx ~style ~bounds ~r ~c:state.cfg.value_scroll str);
      draw_highlight ~ctx ~bounds ~r (i = state.cfg.signal_cursor));
    !max_string_length
  ;;

  let draw_status ?(style = Draw.Style.default) ~ctx ~bounds (state : Waves.t) =
    let style = get_style style in
    fill ~ctx ~bounds ~style ' ';
    draw_string
      ~ctx
      ~style
      ~bounds
      ~r:0
      ~c:0
      (Printf.sprintf
         "cycle=%i cursor=%i w=%i h=%i sc=%i vs=%i"
         state.cfg.start_cycle
         state.cfg.wave_cursor
         state.cfg.wave_width
         state.cfg.wave_height
         state.cfg.signal_scroll
         state.cfg.value_scroll)
  ;;

  let draw_ui ?(style = Styles.default Draw.Style.default) ?bounds ~ctx (state : Waves.t)
    =
    let open Styles in
    let open Bounds in
    let bounds =
      match bounds with
      | None -> fit_to_window (get_bounds ctx)
      | Some b -> b
    in
    with_border
      ~draw:draw_signals
      ~label:"Signals"
      ~style:style.signals
      ?border:style.border
      ~ctx
      ~bounds:bounds.signals
      state;
    ignore
      (with_border
         ~draw:draw_values
         ~label:"Values"
         ~style:style.values
         ?border:style.border
         ~ctx
         ~bounds:bounds.values
         state
       : int);
    with_border
      ~draw:draw_wave
      ~label:"Waves"
      ~style:style.waves
      ?border:style.border
      ~ctx
      ~bounds:bounds.waves
      state;
    with_border
      ~draw:draw_status
      ~label:"Status"
      ~style:style.status
      ?border:style.border
      ~ctx
      ~bounds:bounds.status
      state
  ;;

  type pick =
    | Wave of int * int
    | Value of int
    | Signal of int
    | Status
    | No_pick

  let pick ~bounds ~r ~c (state : Waves.t) =
    let open Draw in
    let open Bounds in
    let in_rect b = r >= b.r && c >= b.c && r < b.r + b.h && c < b.c + b.w in
    let get_signal_offset b =
      let r = r - b.r in
      let rec f row i =
        if i < Array.length state.waves
        then (
          let _, wah = get_wave_height (state.cfg.wave_height, state.waves.(i)) in
          if r >= row && r < row + wah then i else f (row + wah) (i + 1))
        else 0
        (* better default? *)
      in
      f 0 state.cfg.start_signal
    in
    let get_wave_offset b =
      let c = c - b.c in
      let w, waw = get_wave_width (state.cfg.wave_width, Clock "") in
      let w_scale = get_w_scale w in
      (c / waw * w_scale) + state.cfg.start_cycle
    in
    if in_rect bounds.waves
    then Wave (get_wave_offset bounds.waves, get_signal_offset bounds.waves)
    else if in_rect bounds.values
    then Value (get_signal_offset bounds.values)
    else if in_rect bounds.signals
    then Signal (get_signal_offset bounds.signals)
    else if in_rect bounds.status
    then Status
    else No_pick
  ;;
end

module Static = struct
  module R = Make (Draw.In_memory)

  let border_ext = function
    | None -> 0
    | Some _ -> 2
  ;;

  let get_max_height border (state : Waves.t) =
    border_ext border + R.get_max_wave_height state state.cfg.start_signal
  ;;

  let draw
        ?signals
        ?values
        ?waves
        ?(style = Styles.default Draw.Style.default)
        ?rows
        ?cols
        state
    =
    (* inferred width and height *)
    let cols =
      match cols with
      | None -> 80
      | Some x -> x
    in
    let rows =
      match rows with
      | None -> get_max_height style.Styles.border state
      | Some x -> x
    in
    (* do drawing *)
    let ctx = Draw.In_memory.init ~rows ~cols in
    let bounds =
      Bounds.fit_to_window
        ?signals
        ?values
        ?waves
        Draw.{ r = 0; c = 0; h = rows; w = cols }
    in
    R.draw_ui ~style ~ctx ~bounds state;
    (* return context *)
    ctx
  ;;

  let draw_full ?(style = Styles.default Draw.Style.default) state =
    let open Bounds in
    let open Styles in
    let bounds = R.get_max_bounds state in
    let ext = border_ext style.border in
    let get_ctx b =
      let open Draw in
      let b = { b with w = b.w + ext; h = b.h + ext } in
      let ctx = Draw.In_memory.init ~rows:b.h ~cols:b.w in
      let b = if ext = 0 then b else Bounds.shrink_for_border b in
      b, ctx
    in
    let b, sctx = get_ctx bounds.signals in
    R.with_border
      ~draw:R.draw_signals
      ?border:style.border
      ~label:"Signals"
      ~style:style.signals
      ~ctx:sctx
      ~bounds:b
      state;
    let b, vctx = get_ctx bounds.values in
    ignore
    @@ R.with_border
         ~draw:R.draw_values
         ?border:style.border
         ~label:"Values"
         ~style:style.values
         ~ctx:vctx
         ~bounds:b
         state;
    let b, wctx = get_ctx bounds.waves in
    R.with_border
      ~draw:R.draw_wave
      ?border:style.border
      ~label:"Waves"
      ~style:style.waves
      ~ctx:wctx
      ~bounds:b
      state;
    sctx, vctx, wctx
  ;;
end
