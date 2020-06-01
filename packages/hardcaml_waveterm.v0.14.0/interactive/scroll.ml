open! Import

module Adjustment = struct
  type t =
    { mutable range : int
    ; mutable offset : int
    ; mutable on_offset_change : (int -> unit[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create () = { range = 0; offset = 0; on_offset_change = (fun _ -> ()) }

  let set_offset ?(trigger_callback = true) t o =
    let o' = max 0 (min (t.range - 1) o) in
    if t.offset <> o'
    then (
      t.offset <- o';
      if trigger_callback then t.on_offset_change o')
  ;;

  let set_range ?(trigger_callback = true) t r =
    t.range <- max 0 r;
    set_offset ~trigger_callback t t.offset
  ;;
end

module Scroll_bar_mode = struct
  type t =
    | Fixed of int
    | Dynamic of int
  [@@deriving sexp_of]
end

module Mouse_mode = struct
  type t =
    | Middle
    | Ratio
    | Auto
  [@@deriving sexp_of]
end

let map_range range1 range2 offset1 =
  if range1 = 0
  then 0
  else (
    let map_range range1 range2 offset1 =
      Float.(max 0. (min range2 (range2 *. offset1 /. range1)))
    in
    let rnd x = Float.to_int (x +. 0.5) in
    rnd @@ map_range (Float.of_int range1) (Float.of_int range2) (Float.of_int offset1))
;;

module Scrollable = struct
  type t =
    { adj : Adjustment.t
    ; mutable scroll_window_size : int
    ; mutable scroll_bar_mode : Scroll_bar_mode.t
    ; mutable min_scroll_bar_size : int option
    ; mutable max_scroll_bar_size : int option
    ; mutable scroll_bar_size : int
    ; mutable scroll_bar_offset : int
    ; mutable mouse_mode : Mouse_mode.t
    ; mutable page_size : int
    ; mutable document_size : int
    ; mutable on_scrollbar_change : (unit -> unit[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create () =
    { adj = Adjustment.create ()
    ; scroll_window_size = 0
    ; scroll_bar_mode = Fixed 1
    ; min_scroll_bar_size = None
    ; max_scroll_bar_size = None
    ; scroll_bar_size = 0
    ; scroll_bar_offset = 0
    ; mouse_mode = Middle
    ; page_size = -1
    ; document_size = -1
    ; on_scrollbar_change = Fn.id
    }
  ;;

  let scroll_bar_steps t = t.scroll_window_size - t.scroll_bar_size + 1

  let set_scroll_bar_offset t o =
    let offset = max 0 (min (scroll_bar_steps t - 1) o) in
    if t.scroll_bar_offset <> offset
    then (
      t.scroll_bar_offset <- offset;
      t.on_scrollbar_change ())
  ;;

  let scroll_of_window t offset =
    let offset = map_range (t.adj.range - 1) (scroll_bar_steps t - 1) offset in
    offset
  ;;

  let set_offset ?(trigger_callback = true) t o =
    Adjustment.set_offset t.adj ~trigger_callback o;
    set_scroll_bar_offset t (scroll_of_window t t.adj.offset)
  ;;

  let set_range ?(trigger_callback = true) t r =
    Adjustment.set_range ~trigger_callback t.adj r;
    set_scroll_bar_offset t (scroll_of_window t t.adj.offset)
  ;;

  let scroll_bar_size_fixed t size =
    let wsize = t.scroll_window_size in
    if wsize <= size then max 1 (wsize - 1) else max 1 size
  ;;

  let scroll_bar_size_dynamic t view_size =
    if t.adj.range <= 1
    then t.scroll_window_size
    else if view_size <= 0
    then max 1 (t.scroll_window_size / max 1 t.adj.range)
    else (
      let range = Float.of_int t.adj.range in
      let scroll_size = Float.of_int @@ t.scroll_window_size in
      let view_size = Float.of_int view_size in
      let doc_size = view_size +. range in
      Float.to_int @@ (scroll_size *. view_size /. doc_size))
  ;;

  let min_scroll_bar_size t =
    match t.min_scroll_bar_size with
    | None -> 1
    | Some x -> x
  ;;

  let max_scroll_bar_size t =
    match t.max_scroll_bar_size with
    | None -> t.scroll_window_size
    | Some x -> x
  ;;

  let scroll_bar_size t =
    let size =
      max (min_scroll_bar_size t)
      @@ min (max_scroll_bar_size t)
      @@
      match t.scroll_bar_mode with
      | Fixed size -> scroll_bar_size_fixed t size
      | Dynamic size -> scroll_bar_size_dynamic t size
    in
    if t.scroll_bar_size <> size
    then (
      t.scroll_bar_size <- size;
      t.on_scrollbar_change ());
    size
  ;;

  let _set_scroll_bar_offset t o =
    let offset = max 0 (min (scroll_bar_steps t - 1) o) in
    if t.scroll_bar_offset <> offset
    then (
      t.scroll_bar_offset <- offset;
      t.on_scrollbar_change ())
  ;;

  let window_of_scroll t offset =
    map_range (scroll_bar_steps t - 1) (t.adj.range - 1) offset
  ;;

  let _scroll_of_window t offset =
    let offset = map_range (t.adj.range - 1) (scroll_bar_steps t - 1) offset in
    offset
  ;;

  let incr t =
    if t.adj.range >= scroll_bar_steps t
    then window_of_scroll t (t.scroll_bar_offset + 1)
    else t.adj.offset + 1
  ;;

  let decr t =
    if t.adj.range >= scroll_bar_steps t
    then window_of_scroll t (t.scroll_bar_offset - 1)
    else t.adj.offset - 1
  ;;

  let mouse_scale_ratio t scroll =
    let steps = scroll_bar_steps t in
    let wsize = t.scroll_window_size in
    let dead_zone = wsize / 5 in
    (* ~10% at each end *)
    map_range (wsize - dead_zone - 1) (steps - 1) (scroll - (dead_zone / 2))
  ;;

  let mouse_scale_middle t scroll =
    let size = scroll_bar_size t in
    scroll - (size / 2)
  ;;

  let mouse_scale_auto t scroll =
    if scroll_bar_size t > t.scroll_window_size / 2
    then mouse_scale_ratio t scroll
    else mouse_scale_middle t scroll
  ;;

  let scroll_of_mouse t scroll =
    match t.mouse_mode with
    | Middle -> mouse_scale_middle t scroll
    | Ratio -> mouse_scale_ratio t scroll
    | Auto -> mouse_scale_auto t scroll
  ;;

  let mouse_scroll t scroll = scroll_of_mouse t scroll |> window_of_scroll t
  let calculate_range page_size document_size = document_size - page_size + 1

  let update_page_and_document_sizes t page doc =
    if t.page_size <> page || t.document_size <> doc
    then (
      t.page_size <- page;
      t.document_size <- doc;
      let range = max 0 (calculate_range t.page_size t.document_size) in
      set_range t range;
      t.mouse_mode <- Auto;
      t.scroll_bar_mode <- Dynamic t.page_size)
  ;;

  let _set_page_size t s = update_page_and_document_sizes t s t.document_size
  let _set_document_size t s = update_page_and_document_sizes t t.page_size s
  let _page_prev t = t.adj.offset - t.page_size
  let _page_next t = t.adj.offset + t.page_size
end

module Scroll_bar_style = struct
  type t =
    | Filled
    | Outline
  [@@deriving sexp_of]
end

module Orientation = struct
  type t =
    | Horz
    | Vert
  [@@deriving sexp_of]
end

module Scrollbar = struct
  type t =
    { scrollable : Scrollable.t
    ; mutable bar_style : Scroll_bar_style.t
    ; incr_key : (Notty.Unescape.key[@sexp.opaque])
    ; decr_key : (Notty.Unescape.key[@sexp.opaque])
    ; mutable bounds : Draw.rect
    ; orientation : Orientation.t
    }
  [@@deriving sexp_of]

  let hbar = 0x2550
  let vbar = 0x2551
  let filled = 0x2588

  let mouse_offset (t : t) ~row ~col =
    match t.orientation with
    | Horz -> col - t.bounds.c
    | Vert -> row - t.bounds.r
  ;;

  let set_bounds t (bounds : Draw.rect) =
    (match t.orientation with
     | Horz -> t.scrollable.scroll_window_size <- bounds.w
     | Vert -> t.scrollable.scroll_window_size <- bounds.h);
    t.bounds <- bounds
  ;;

  let create
        ?(bar_style = Scroll_bar_style.Filled)
        ~incr_key
        ~decr_key
        ~orientation
        ~bounds
        ()
    =
    let scrollable = Scrollable.create () in
    let scrollbar = { scrollable; bar_style; incr_key; decr_key; bounds; orientation } in
    set_bounds scrollbar bounds;
    scrollbar
  ;;

  let key_event t (key : Notty.Unescape.key) =
    if Poly.equal key t.incr_key
    then (
      Scrollable.set_offset t.scrollable (Scrollable.incr t.scrollable);
      true)
    else if Poly.equal key t.decr_key
    then (
      Scrollable.set_offset t.scrollable (Scrollable.decr t.scrollable);
      true)
    else false
  ;;

  let mouse_event t ((ev, (col, row), mods) : Notty.Unescape.mouse) =
    match ev, mods with
    | `Press `Left, [] ->
      let scroll = mouse_offset t ~row ~col in
      Scrollable.set_offset t.scrollable (Scrollable.mouse_scroll t.scrollable scroll);
      true
    | _ -> false
  ;;

  let draw_bar ~ctx ~style ~(bounds : Draw.rect) (t : t) =
    let cols, rows = bounds.w, bounds.h in
    let is_filled =
      match t.bar_style with
      | Filled -> true
      | Outline -> false
    in
    if cols = 1 || rows = 1 || is_filled
    then (
      let x =
        match t.bar_style with
        | Filled -> filled
        | Outline -> if cols = 1 then vbar else hbar
      in
      for c = 0 to bounds.w - 1 do
        for r = 0 to bounds.h - 1 do
          Draw_notty.draw_int ~ctx ~style ~bounds ~r ~c x
        done
      done)
    else Draw_notty.draw_box ~ctx ~style ~bounds ""
  ;;
end

module VScrollbar = struct
  type t = Scrollbar.t [@@deriving sexp_of]

  let create (bounds : Draw.rect) =
    Scrollbar.create
      ~incr_key:(`Arrow `Down, [])
      ~decr_key:(`Arrow `Up, [])
      ~bounds
      ~orientation:Vert
      ()
  ;;

  let draw ~ctx ~style (t : t) =
    let scroll_bar_size = Scrollable.scroll_bar_size t.scrollable in
    let scroll_offset = t.scrollable.scroll_bar_offset in
    let bounds = t.bounds in
    Draw_notty.fill ~ctx ~style ~bounds ' ';
    let bounds =
      { Draw.r = bounds.r + scroll_offset
      ; c = bounds.c
      ; h = scroll_bar_size
      ; w = bounds.w
      }
    in
    Scrollbar.draw_bar ~ctx ~style ~bounds t
  ;;
end

module HScrollbar = struct
  type t = Scrollbar.t [@@deriving sexp_of]

  let create (bounds : Draw.rect) =
    Scrollbar.create
      ~incr_key:(`Arrow `Right, [])
      ~decr_key:(`Arrow `Left, [])
      ~bounds
      ~orientation:Horz
      ()
  ;;

  let draw ~ctx ~style (t : t) =
    let scroll_bar_size = Scrollable.scroll_bar_size t.scrollable in
    let scroll_offset = t.scrollable.scroll_bar_offset in
    let bounds = t.bounds in
    Draw_notty.fill ~ctx ~style ~bounds ' ';
    let bounds =
      { Draw.r = bounds.r
      ; c = bounds.c + scroll_offset
      ; h = bounds.h
      ; w = scroll_bar_size
      }
    in
    Scrollbar.draw_bar ~ctx ~style ~bounds t
  ;;
end
