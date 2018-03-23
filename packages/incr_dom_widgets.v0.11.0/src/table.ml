open! Core_kernel
open! Import
open Vdom

include Table_intf

let cutoff compare x =
  Incr.set_cutoff x (Incr.Cutoff.of_compare compare);
  x

module Make (Row_id : Id) (Column_id : Id) (Sort_spec : Sort_spec) = struct
  include Util

  module Row_id = Row_id
  module Column_id = Column_id

  module Sort_spec = Sort_spec
  module Sort_key  = Sort_spec.Sort_key
  module Sort_dir  = Sort_spec.Sort_dir

  module Sort_criteria = struct
    module By_column = struct
      type 'a t =
        { column : 'a
        ; dir    : Sort_dir.t
        } [@@deriving fields, compare, sexp]
    end

    type 'a t = 'a By_column.t list [@@deriving compare, sexp]

    let map : 'a t -> f:('a -> 'b) -> 'b t = fun t ~f ->
      List.map t ~f:(fun (by_column:_ By_column.t) ->
        { by_column with column = f by_column.column }
      )

    let filter_map : 'a t -> f:('a -> 'b option) -> 'b t = fun t ~f ->
      List.filter_map t ~f:(fun (by_column:_ By_column.t) ->
        Option.map (f by_column.column) ~f:(fun column -> { by_column with column })
      )
  end

  module Base_sort_criteria = struct
    type t = Column_id.t Sort_criteria.By_column.t list [@@deriving compare, sexp]

    let none = []

    let find_precedence_and_dir t target =
      List.find_mapi t ~f:(fun i { Sort_criteria.By_column. column; dir } ->
        Option.some_if (Column_id.equal column target) (i + 1, dir)
      )

    let remove t target =
      List.filter t ~f:(fun { Sort_criteria.By_column. column; dir = _ } ->
        not (Column_id.equal column target)
      )

    let add_to_front t column dir = { Sort_criteria.By_column. column; dir } :: t
  end

  module Column = struct
    type 'a t =
      { header: Node.t
      ; group: string option
      ; sort_by: ('a -> Sort_key.t) option
      } [@@deriving fields]

    let create ?group ?sort_by ~header () =
      { header
      ; group
      ; sort_by
      }
  end

  module Row_node_spec = Row_node_spec

  module Visibility_info = struct
    type t =
      { tbody_rect : float Js_misc.Rect.t
      ; view_rect  : float Js_misc.Rect.t
      } [@@deriving compare, sexp, fields]
  end

  module Key = struct
    module T = struct
      type t =
        { sort_criteria : Sort_key.t option Lazy.t Sort_criteria.t
        ; row_id        : Row_id.t
        } [@@deriving sexp, fields]

      let sort_keys t = List.map t.sort_criteria ~f:Sort_criteria.By_column.column
      let sort_dirs t = List.map t.sort_criteria ~f:Sort_criteria.By_column.dir

      (** The comparison function here is written so that any two keys with the same
          sort_dir sort according to that sort_dir; but keys with different sort_dirs just
          have a stable relation between them. This allows us to have one Key type that is
          used by all the different sorting situations, without needing to change
          comparators. *)
      let compare t1 t2 =
        match t1.sort_criteria, t2.sort_criteria with
        | []  , []   -> Row_id.compare t1.row_id t2.row_id
        | _::_, []   -> -1
        | []  , _::_ ->  1
        | b::_, _::_ ->
          let module B = Sort_criteria.By_column in
          let compare_by_col =
            Comparable.lexicographic
              [ (fun b1 b2 -> Sort_dir.compare b1.B.dir b2.B.dir)
              ; (fun b1 b2 ->
                   match force b1.B.column, force b2.B.column with
                   | None   , None    ->  0
                   | Some _ , None    -> -1
                   | None   , Some _  ->  1
                   | Some k1, Some k2 -> Sort_spec.compare_keys b1.B.dir k1 k2
                )
              ]
          in
          let compare_if_equal_keys =
            Sort_spec.compare_rows_if_equal_keys ~cmp_row_id:Row_id.compare b.B.dir
          in
          Comparable.lexicographic
            [ (fun t1 t2 -> List.compare compare_by_col t1.sort_criteria t2.sort_criteria)
            ; (fun t1 t2 -> compare_if_equal_keys t1.row_id t2.row_id)
            ]
            t1 t2

      let create sort_criteria row_id = { sort_criteria; row_id }
    end
    include T
    include Comparable.Make(T)

    let convert_sort_criteria sort_criteria row =
      Sort_criteria.map sort_criteria ~f:(fun column ->
        lazy (Option.map (Column.sort_by column) ~f:(fun sort_by -> sort_by row))
      )

    let sort sort_criteria ~(rows : _ Row_id.Map.t Incr.t) =
      let create_key row_id data =
        create (convert_sort_criteria sort_criteria data) row_id
      in
      Incr.Map.unordered_fold rows
        ~init:Map.empty
        ~add:(fun ~key:row_id ~data acc ->
          let key = create_key row_id data in
          Map.set acc ~key ~data
        )
        ~remove:(fun ~key:row_id ~data acc ->
          let key = create_key row_id data in
          Map.remove acc key
        )
    ;;
  end

  module Html_id = struct
    type t = string [@@deriving compare, sexp]

    (* This module avoids using [sprintf] for performance reasons *)

    let table         table_id = "table-" ^ Table_id.to_string table_id
    let tbody         table_id = table table_id ^ "-body"
    let thead         table_id = table table_id ^ "-header"
    let column_group  table_id = table table_id ^ "-column-group"
    let column_header table_id = table table_id ^ "-column-header"

    let column_header_cell table_id column_id =
      table table_id ^ "-header-cell-" ^ Column_id.to_string column_id

    let row table_id row_id =
      table table_id ^ "-row-" ^ Row_id.to_string row_id

    let cell_of_parts row_html_id column_id_str =
      row_html_id ^ "-col-" ^ column_id_str

    let cell table_id row_id column_id =
      cell_of_parts (row table_id row_id) (Column_id.to_string column_id)

    let spacer key = "spacer-" ^ key
  end

  module Row_view = Partial_render_list.Make(Key)

  module Model = struct
    type t = { id: Table_id.t (** To avoid DOM id collisions. Never changes. *)
             (** Settings from client. Never change *)
             ; float_header: Float_type.t
             ; float_first_col: Float_type.t
             ; scroll_margin: Margin.t
             ; scroll_region : Scroll_region.Id.t
             (** UI state. Changed by user during app usage *)
             ; focus_row: Row_id.t option
             ; focus_col: Column_id.t option
             ; sort_criteria: Base_sort_criteria.t
             (** Info measured from render. Changes each render. *)
             ; height_cache: Row_view.Height_cache.t
             ; visibility_info: Visibility_info.t option
             ; col_group_row_height: int
             ; tbody_html_id : Html_id.t
             ; thead_html_id : Html_id.t
             ; column_group_html_id : Html_id.t
             ; column_header_html_id : Html_id.t
             }
    [@@deriving fields, compare, sexp_of]

    let create ~scroll_margin ~scroll_region ~float_header ~float_first_col ~height_guess
          ?id ?(initial_sort=Base_sort_criteria.none) ?initial_focus_row
          ?initial_focus_col () =
      let id =
        match id with
        | Some id -> id
        | None    -> Table_id.create ()
      in
      { id
      ; float_header
      ; float_first_col
      ; scroll_margin
      ; scroll_region
      ; focus_row = initial_focus_row
      ; focus_col = initial_focus_col
      ; sort_criteria = initial_sort
      ; height_cache = Row_view.Height_cache.empty ~height_guess
      ; visibility_info = None
      ; col_group_row_height = 0
      ; tbody_html_id = Html_id.tbody id
      ; thead_html_id = Html_id.thead id
      ; column_group_html_id = Html_id.column_group id
      ; column_header_html_id = Html_id.column_header id
      }

    let sort_dirs    t = List.map t.sort_criteria ~f:Sort_criteria.By_column.dir
    let sort_columns t = List.map t.sort_criteria ~f:Sort_criteria.By_column.column

    let set_sort_criteria = Field.fset Fields.sort_criteria

    let set_float_header = Field.fset Fields.float_header

    let cycle_sorting ?keep_existing_cols t column_id ~next_dir =
      let prev_dir =
        Base_sort_criteria.find_precedence_and_dir t.sort_criteria column_id
        |> Option.map ~f:snd
      in
      let cleared_sort_criteria =
        match keep_existing_cols with
        | None    -> Base_sort_criteria.none
        | Some () -> Base_sort_criteria.remove t.sort_criteria column_id
      in
      let sort_criteria =
        match next_dir prev_dir with
        | None     -> cleared_sort_criteria
        | Some dir ->
          Base_sort_criteria.add_to_front cleared_sort_criteria column_id dir
      in
      { t with sort_criteria }

    let get_tbody_rect t =
      Option.map t.visibility_info ~f:Visibility_info.tbody_rect
  end

  module Derived_model = struct
    type 'a t =
      { rows              : 'a Row_id.Map.t
      ; sorted_rows       : 'a Key.Map.t
      ; columns           : (Column_id.t * 'a Column.t) Int.Map.t
      ; column_num_lookup : int Column_id.Map.t
      ; sort_criteria     : 'a Column.t Sort_criteria.t
      ; row_view          : 'a Row_view.t
      ; scroll_region     : Scroll_region.t option
      ; has_col_groups    : bool
      ; floating_col      : Column_id.t option
      } [@@deriving fields]

    let create m ~rows ~(columns : (Column_id.t * _ Column.t) list Incr.t) =
      let scroll_region =
        (* This needs to fire whenever the model or rows change so that it can actually
           find the element [scroll_region] after it is drawn. *)
        let%map m = m
        and _ = rows
        in
        Scroll_region.of_id (Model.scroll_region m)
      in
      let sort_criteria =
        let%map sort_criteria =
          m >>| Model.sort_criteria |> cutoff [%compare: Base_sort_criteria.t]
        and columns = columns
        in
        Sort_criteria.filter_map sort_criteria ~f:(fun column_id ->
          List.find_map columns ~f:(fun (id, c) ->
            if [%compare.equal:Column_id.t] id column_id
            then (Some c) else None
          )
        )
      in
      let sorted_rows = sort_criteria >>= Key.sort ~rows in
      let measurements =
        let%map visibility_info = m >>| Model.visibility_info in
        Option.map visibility_info
          ~f:(fun { Visibility_info. tbody_rect; view_rect; _ } ->
            { Partial_render_list.Measurements. list_rect = tbody_rect ; view_rect }
          )
      in
      let floating_col =
        let%map float_first_col = m >>| Model.float_first_col
        and columns = columns
        in
        if Float_type.is_floating float_first_col
        then (Option.map (List.hd columns) ~f:fst)
        else None
      in
      let height_cache = m >>| Model.height_cache in
      let column_num_lookup =
        let%map columns = columns in
        Column_id.Map.of_alist_exn (List.mapi columns ~f:(fun i (col_id, _) -> col_id, i))
      in
      let has_col_groups =
        let%map columns = columns in
        List.exists columns ~f:(fun (_, col) -> Option.is_some col.group)
      in
      let columns =
        let%map columns = columns in
        Int.Map.of_alist_exn (List.mapi columns ~f:(fun i col -> i, col))
      in
      let%map row_view = Row_view.create ~rows:sorted_rows ~height_cache ~measurements
      and rows = rows
      and sorted_rows = sorted_rows
      and columns = columns
      and column_num_lookup = column_num_lookup
      and sort_criteria = sort_criteria
      and scroll_region = scroll_region
      and floating_col = floating_col
      and has_col_groups = has_col_groups
      in
      { rows
      ; sorted_rows
      ; columns
      ; column_num_lookup
      ; sort_criteria
      ; row_view
      ; scroll_region
      ; has_col_groups
      ; floating_col
      }
    ;;
  end

  module Action = struct
    type t =
      | Sort_column_clicked of Column_id.t
      | Move_focus_row of Focus_dir.t
      | Move_focus_col of Focus_dir.t
      | Set_focus_row of Row_id.t option
      | Set_focus_col of Column_id.t option
      | Page_focus_row of Focus_dir.t
    [@@deriving sexp, compare, variants]
  end

  type 'a row_renderer
    =  row_id:Row_id.t
    -> row:'a Incr.t
    -> Row_node_spec.t Incr.t

  let current_key (d : _ Derived_model.t) ~row_id =
    let open Option.Let_syntax in
    let%map row = Row_id.Map.find d.rows row_id in
    let sort_criteria = Key.convert_sort_criteria d.sort_criteria row in
    Key.create sort_criteria row_id
  ;;

  let move_focus_row m (d : _ Derived_model.t) ~dir =
    let focus_row =
      let open Option.Let_syntax in
      let focus_key =
        let%bind row_id = m.Model.focus_row in
        current_key d ~row_id
      in
      let%map ({row_id; _}, _) = Util.move_focus d.sorted_rows focus_key dir in
      row_id
    in
    if Option.is_some focus_row
    then { m with focus_row }
    else m
  ;;

  let move_focus_col m (d : _ Derived_model.t) ~dir =
    let focus_col =
      let open Option.Let_syntax in
      let focus_key =
        let%bind col_id = m.Model.focus_col in
        Map.find d.column_num_lookup col_id
      in
      let%map (_, (col_id, _)) = Util.move_focus d.columns focus_key dir in
      col_id
    in
    if Option.is_some focus_col
    then { m with focus_col }
    else m
  ;;

  let set_focus_row (m : Model.t) row_id =
    if [%compare.equal:Row_id.t option] m.focus_row row_id
    then m
    else { m with focus_row = row_id }
  ;;

  let set_focus_col (m : Model.t) col_id =
    if [%compare.equal:Column_id.t option] m.focus_col col_id
    then m
    else { m with focus_col = col_id }
  ;;

  (* Possible offset due to floating header *)
  let get_top_margin_offset (m : Model.t) =
    let get_float_elem_size () =
      Option.map (Dom_html.getElementById_opt m.thead_html_id)
        ~f:(fun el -> Js_misc.viewport_rect_of_element el |> Js_misc.Rect.float_height)
    in
    Float_type.compute_offset m.float_header ~get_float_elem_size
  ;;

  (* Possible offset due to floating first column *)
  let get_left_margin_offset (m : Model.t) (d : _ Derived_model.t) ~is_floating_col =
    if is_floating_col
    then 0.
    else (
      let get_float_elem_size () =
        let open Option.Let_syntax in
        let%bind (_, (first_column_id, _)) = Map.min_elt d.columns in
        let%map el =
          Dom_html.getElementById_opt (Html_id.column_header_cell m.id first_column_id)
        in
        Js_misc.viewport_rect_of_element el |> Js_misc.Rect.float_width
      in
      Float_type.compute_offset m.float_first_col ~get_float_elem_size
    )
  ;;

  let call_row_scroll_function d ~row_id ~f =
    Option.map (current_key d ~row_id) ~f:(fun key -> f d.row_view ~key)
  ;;

  let is_floating_col (d : _ Derived_model.t) column_id =
    [%compare.equal: Column_id.t option] d.floating_col (Some column_id)
  ;;

  let call_col_scroll_function ?f_if_currently_floating (m : Model.t) ~column_id ~f
        ~is_floating_col =
    let open Option.Let_syntax in
    let%map cell_rect =
      let%map header_cell =
        Dom_html.getElementById_opt (Html_id.column_header_cell m.id column_id)
      in
      Js_misc.viewport_rect_of_element header_cell
    and { tbody_rect; view_rect; _ } = m.visibility_info
    in
    let elem_start, elem_end, is_currently_floating =
      if not is_floating_col
      then (Js_misc.Rect.left cell_rect, Js_misc.Rect.right cell_rect, false)
      else begin
        let left = Js_misc.Rect.left tbody_rect in
        let width = Js_misc.Rect.float_width cell_rect in
        let is_currently_floating =
          match Float_type.px_from_edge m.float_first_col with
          | None    -> false
          | Some px ->
            let float_pos_left  = view_rect.left +. Float.of_int px in
            let float_pos_right = float_pos_left +. width in
            Float.(<=) tbody_rect.left float_pos_left &&
            Float.(>=) tbody_rect.right float_pos_right
        in
        left, left +. width, is_currently_floating
      end
    in
    let f =
      match is_currently_floating, f_if_currently_floating with
      | true, Some f'      -> f'
      | false, _ | _, None -> f
    in
    f ~scroll_region_start:view_rect.left
      ~scroll_region_end:view_rect.right
      ~elem_start
      ~elem_end
  ;;

  let scroll_row_into_scroll_region (m:Model.t) (d : _ Derived_model.t) row_id =
    let top_margin_offset = get_top_margin_offset m in
    let f =
      Row_view.scroll_into_scroll_region
        ?in_:d.scroll_region
        ~top_margin:(m.scroll_margin.top +. top_margin_offset)
        ~bottom_margin:m.scroll_margin.bottom
    in
    Option.value (call_row_scroll_function d ~row_id ~f) ~default:`Didn't_scroll
  ;;

  let scroll_col_into_scroll_region (m:Model.t) (d:_ Derived_model.t) column_id =
    let is_floating_col = is_floating_col d column_id in
    let left_margin_offset = get_left_margin_offset m d ~is_floating_col in
    let f =
      Scroll.scroll_into_region
        ?in_:d.scroll_region
        Horizontal
        ~start_margin:(m.scroll_margin.left +. left_margin_offset)
        ~end_margin:m.scroll_margin.right
    in
    let f_if_currently_floating ~scroll_region_start:_ ~scroll_region_end:_ ~elem_start:_
          ~elem_end:_ =
      `Didn't_scroll
    in
    Option.value
      (call_col_scroll_function m ~column_id ~f ~f_if_currently_floating ~is_floating_col)
      ~default:`Didn't_scroll
  ;;

  let scroll_row_to_position ?keep_in_scroll_region (m : Model.t) (d: _ Derived_model.t)
        row_id ~position =
    let f =
      match keep_in_scroll_region with
      | None    -> Row_view.scroll_to_position ?in_:d.scroll_region ~position
      | Some () ->
        let top_margin_offset = get_top_margin_offset m in
        Row_view.scroll_to_position_and_into_region
          ?in_:d.scroll_region
          ~position
          ~top_margin:(m.scroll_margin.top +. top_margin_offset)
          ~bottom_margin:m.scroll_margin.bottom
    in
    Option.value (call_row_scroll_function d ~row_id ~f) ~default:`Didn't_scroll
  ;;

  let scroll_col_to_position ?keep_in_scroll_region (m:Model.t) (d:_ Derived_model.t)
        column_id ~position =
    let is_floating_col = is_floating_col d column_id in
    let scroll_to_position ~scroll_region_start ~scroll_region_end:_
          ~elem_start ~elem_end:_ =
      Scroll.scroll_to_position
        ?in_:d.scroll_region
        Horizontal
        ~position
        ~scroll_region_start
        ~elem_start
    in
    let scroll_to_position_and_into_region =
      let left_margin_offset = get_left_margin_offset m d ~is_floating_col in
      Scroll.scroll_to_position_and_into_region
        ?in_:d.scroll_region
        Horizontal
        ~position
        ~start_margin:(m.scroll_margin.left +. left_margin_offset)
        ~end_margin:m.scroll_margin.right
    in
    let f, f_if_currently_floating =
      match keep_in_scroll_region with
      | None    -> scroll_to_position                , None
      | Some () -> scroll_to_position_and_into_region, Some scroll_to_position
    in
    Option.value
      (call_col_scroll_function m ~column_id ~f ~is_floating_col ?f_if_currently_floating)
      ~default:`Didn't_scroll
  ;;

  let row_is_in_scroll_region ?scroll_margin (m : Model.t) (d : _ Derived_model.t) row_id =
    let top_margin_offset = get_top_margin_offset m in
    let f =
      let scroll_margin = Option.value scroll_margin ~default:m.scroll_margin in
      Row_view.is_in_region
        ~top_margin:(scroll_margin.top +. top_margin_offset)
        ~bottom_margin:scroll_margin.bottom
    in
    Option.join (call_row_scroll_function d ~row_id ~f)
  ;;

  let col_is_in_scroll_region ?scroll_margin (m : Model.t) (d : _ Derived_model.t)
        column_id =
    let is_floating_col = is_floating_col d column_id in
    let left_margin_offset = get_left_margin_offset m d ~is_floating_col in
    let f =
      let scroll_margin = Option.value scroll_margin ~default:m.scroll_margin in
      Scroll.is_in_region
        ~start_margin:(scroll_margin.left +. left_margin_offset)
        ~end_margin:scroll_margin.right
    in
    let f_if_currently_floating ~scroll_region_start:_ ~scroll_region_end:_ ~elem_start:_
          ~elem_end:_ =
      true
    in
    call_col_scroll_function m ~column_id ~f ~f_if_currently_floating ~is_floating_col
  ;;

  let get_row_position (d:_ Derived_model.t) row_id =
    let f = Row_view.get_position in
    Option.join (call_row_scroll_function d ~row_id ~f)
  ;;

  let get_col_position (m:Model.t) (d: _ Derived_model.t) column_id =
    let is_floating_col = is_floating_col d column_id in
    let f ~scroll_region_start ~scroll_region_end:_ ~elem_start ~elem_end:_ =
      Scroll.get_position ~scroll_region_start ~elem_start
    in
    call_col_scroll_function m ~column_id ~f ~is_floating_col
  ;;

  let get_row_top_and_bottom (d:_ Derived_model.t) row_id =
    let f = Row_view.get_top_and_bottom in
    Option.join (call_row_scroll_function d ~row_id ~f)
  ;;

  let get_col_left_and_right (m:Model.t) (d: _ Derived_model.t) column_id =
    let is_floating_col = is_floating_col d column_id in
    let f ~scroll_region_start ~scroll_region_end:_ ~elem_start ~elem_end =
      let left  = Scroll.get_position ~scroll_region_start ~elem_start in
      left, left +. elem_end -. elem_start
    in
    call_col_scroll_function m ~column_id ~f ~is_floating_col
  ;;

  let scroll_focus_into_scroll_region (m:Model.t) d =
    let row_scroll =
      Option.value_map m.focus_row ~default:`Didn't_scroll
        ~f:(scroll_row_into_scroll_region m d)
    in
    let col_scroll =
      Option.value_map m.focus_col ~default:`Didn't_scroll
        ~f:(scroll_col_into_scroll_region m d)
    in
    Scroll_result.combine row_scroll col_scroll
  ;;

  let scroll_focus_to_position ?keep_in_scroll_region (m:Model.t) d ~position:(x, y) =
    let row_scroll =
      Option.value_map m.focus_row ~default:`Didn't_scroll
        ~f:(scroll_row_to_position ?keep_in_scroll_region m d ~position:y)
    in
    let col_scroll =
      Option.value_map m.focus_col ~default:`Didn't_scroll
        ~f:(scroll_col_to_position ?keep_in_scroll_region m d ~position:x)
    in
    Scroll_result.combine row_scroll col_scroll
  ;;

  let page_focus_row (m : Model.t) (d : _ Derived_model.t) ~(dir : Focus_dir.t) =
    let open Option.Let_syntax in
    let new_focus_row =
      let%bind visibility_info = m.visibility_info and focus_row = m.focus_row in
      let%bind focus_key = current_key d ~row_id:focus_row in
      let focus_height = Row_view.Height_cache.height m.height_cache focus_key in
      let scroll_height = Js_misc.Rect.float_height visibility_info.view_rect in
      let top_margin_offset = get_top_margin_offset m in
      let mult =
        match dir with
        | Prev -> -1.
        | Next ->  1.
      in
      let offset =
        mult *. (scroll_height -. focus_height -. top_margin_offset)
      in
      Row_view.find_by_relative_position d.row_view focus_key ~offset
    in
    match new_focus_row with
    | None     -> m
    | Some row -> set_focus_row m (Some (Key.row_id row))
  ;;

  let focus_is_in_scroll_region ?scroll_margin (m : Model.t) d =
    let row = Option.bind m.focus_row ~f:(row_is_in_scroll_region ?scroll_margin m d) in
    let col =
      Option.bind m.focus_col ~f:(col_is_in_scroll_region ?scroll_margin m d)
    in
    match row, col with
    | None, None       -> None
    | None, Some b
    | Some b, None     -> Some b
    | Some b1, Some b2 -> Some (b1 && b2)
  ;;

  let get_focus_position (m : Model.t) (d : _ Derived_model.t) =
    Option.bind m.focus_col ~f:(get_col_position m d),
    Option.bind m.focus_row ~f:(get_row_position   d)
  ;;

  let get_focus_rect (m : Model.t) (d : _ Derived_model.t) =
    let open Option.Let_syntax in
    let%bind row_id = m.focus_row and col_id = m.focus_col in
    let%bind top , bottom = get_row_top_and_bottom   d row_id in
    let%map  left, right  = get_col_left_and_right m d col_id in
    { Js_misc.Rect. left; right; top; bottom }
  ;;

  let find_row_by_position (m : Model.t) (d : _ Derived_model.t) position =
    let open Option.Let_syntax in
    let%map { Visibility_info. tbody_rect; _ } = m.visibility_info in
    let position = position -. Js_misc.Rect.top tbody_rect in
    if Float.is_negative position
    then `Before
    else (
      match Row_view.find_by_position d.row_view ~position with
      | Some { Key. row_id; _ } -> `At row_id
      | None                    -> `After
    )
  ;;

  let find_col_by_position (m : Model.t) (d : _ Derived_model.t) position =
    let open Option.Let_syntax in
    List.fold_until (Map.data d.columns) ~init:true
      ~f:(fun is_first (col_id, _) ->
        let col_header_rect =
          let html_id = Html_id.column_header_cell m.id col_id in
          let%map elem = Dom_html.getElementById_opt html_id in
          Js_misc.viewport_rect_of_element elem
        in
        match col_header_rect with
        | None      -> Stop None
        | Some rect ->
          if is_first && position < Js_misc.Rect.left rect
          then (Stop (Some `Before))
          else if position <= Js_misc.Rect.right rect
          then (Stop (Some (`At col_id)))
          else (Continue false)
      )
      ~finish:(function
        | false  -> Some `After
        | true   ->
          let%map { Visibility_info. tbody_rect; _ } = m.visibility_info in
          if Float.(<) position (Js_misc.Rect.left tbody_rect)
          then `Before
          else `After
      )
  ;;

  let on_display ~(old:Model.t) (model:Model.t) d =
    if old.focus_row <> model.focus_row || old.focus_col <> model.focus_col
    then (
      let maybe_scroll x f =
        Option.iter x ~f:(fun x -> ignore (f model d x : Scroll_result.t))
      in
      maybe_scroll model.focus_row scroll_row_into_scroll_region ;
      maybe_scroll model.focus_col scroll_col_into_scroll_region ;
    )
  ;;

  let sort_column_clicked =
    Model.cycle_sorting ~next_dir:Sort_dir.next
  ;;

  let apply_action m d (action : Action.t) =
    match action with
    | Sort_column_clicked column_id -> sort_column_clicked m column_id
    | Move_focus_row dir -> move_focus_row m d ~dir
    | Move_focus_col dir -> move_focus_col m d ~dir
    | Set_focus_row row_id -> set_focus_row m row_id
    | Set_focus_col col_id -> set_focus_col m col_id
    | Page_focus_row dir -> page_focus_row m d ~dir
  ;;

  (** returns the element associated with the row id in question  *)
  let find_row_element table_id row_id =
    Dom_html.getElementById_opt (Html_id.row table_id row_id)

  let update_col_group_row_height (m : Model.t) (d : _ Derived_model.t) =
    let has_floating_header () = Float_type.is_floating m.float_header in
    let height =
      if not (d.has_col_groups && has_floating_header ())
      then None
      else (
        let open Option.Let_syntax in
        let%map column_group  = Dom_html.getElementById_opt (Html_id.column_group m.id)
        and     column_header = Dom_html.getElementById_opt (Html_id.column_header m.id)
        in
        (* We don't use [Js_misc.viewport_rect_of_element] here so that we can round down
           instead of rounding to the nearest interger. This reduces jitter. *)
        let column_group_top  = column_group##getBoundingClientRect##.top  in
        let column_header_top = column_header##getBoundingClientRect##.top in
        int_of_float (column_header_top -. column_group_top)
      )
    in
    Option.value height ~default:0

  (** Computes and updates the heights of all rows that are currently rendered *)
  let update_height_cache (m : Model.t) (d : _ Derived_model.t) =
    Row_view.measure_heights
      d.row_view
      ~measure_row:(fun key ->
        Option.map (find_row_element m.id key.row_id) ~f:(fun el ->
          let rect = Js_misc.viewport_rect_of_element el in
          Js_misc.Rect.top rect, Js_misc.Rect.bottom rect
        )
      )
      ~get_row_height:(fun ~prev ~curr ~next ->
        Option.map curr ~f:(fun (curr_top, curr_bottom) ->
          let with_top_margin =
            Option.map (Option.map prev ~f:Tuple2.get2)
              ~f:(fun prev_bottom -> curr_bottom -. prev_bottom)
          in
          let with_bottom_margin =
            Option.map (Option.map next ~f:Tuple2.get1)
              ~f:(fun next_top -> next_top -. curr_top)
          in
          match with_top_margin, with_bottom_margin with
          | Some h1, Some h2 -> (h1 +. h2) /. 2.
          | Some h, None
          | None, Some h     -> h
          | None, None       -> curr_bottom -. curr_top
        )
      )

  let update_visibility_info (m : Model.t) (d : _ Derived_model.t) =
    let open Option.Let_syntax in
    let scroll_region =
      match d.scroll_region with
      | Some _ as a -> a
      | None ->
        Scroll_region.of_id (Model.scroll_region m)
    in
    let%map scroll_region = scroll_region
    and tbody = Dom_html.getElementById_opt m.tbody_html_id
    in
    let view_rect =
      match scroll_region with
      | Window     -> Js_misc.Rect.map (Js_misc.client_rect ()) ~f:Float.of_int
      | Element el -> Js_misc.client_rect_of_element el
    in
    { Visibility_info.
      tbody_rect = Js_misc.client_rect_of_element tbody
    ; view_rect
    }

  let update_visibility (m : Model.t) d =
    let visibility_info      = update_visibility_info      m d in
    let height_cache         = update_height_cache         m d in
    let col_group_row_height = update_col_group_row_height m d in
    if [%compare.equal: Visibility_info.t option] visibility_info m.visibility_info
    && [%compare.equal: Row_view.Height_cache.t] height_cache m.height_cache
    && [%compare.equal: int] col_group_row_height m.col_group_row_height
    then m
    else { m with visibility_info; height_cache; col_group_row_height }

  let px_of_int x =
    Int.to_string x ^ "px"

  let px_of_float x =
    px_of_int (Float.iround_nearest_exn x)

  let spacer ~key =
    let id_attr = Attr.id (Html_id.spacer key) in
    stage (fun height ->
      [Node.tr ~key [ id_attr; Attr.style [ "height", px_of_float height ]] []]
    )

  let sticky_pos side (pos:Float_type.t Incr.t) =
    let%map pos = pos >>| Float_type.px_from_edge in
    Option.map pos ~f:(fun pos -> side, pos)

  let finalize_sticky_pos sticky_pos =
    Option.map sticky_pos ~f:(fun (side, pos) -> side, sprintf "%dpx" pos)

  let sticky_style ~sticky_pos ~z_index =
    let sticky_style =
      if List.is_empty sticky_pos
      then []
      else (("position", "sticky") :: sticky_pos)
    in
    let z_index_style = "z-index", Int.to_string z_index in
    z_index_style :: sticky_style

  let view_header ?override_on_click ~inject ~columns ~top_sticky_pos ~left_sticky_pos m =
    let get_sticky_attrs ~top_sticky_pos =
      let first_cell =
        sticky_style ~sticky_pos:(List.filter_opt [ left_sticky_pos; top_sticky_pos ])
          ~z_index:3
        |> Attr.style
      in
      let default =
        sticky_style ~sticky_pos:(Option.to_list top_sticky_pos) ~z_index:2
        |> Attr.style
      in
      first_cell, default
    in
    let header_nodes =
      let%map sort_criteria = m >>| Model.sort_criteria
      and id = m >>| Model.id
      and columns = columns
      and top_sticky_pos =
        match top_sticky_pos with
        | None           -> Incr.return None
        | Some (dir, px) ->
          let%map col_group_row_height = m >>| Model.col_group_row_height in
          finalize_sticky_pos (Some (dir, px + col_group_row_height))
      in
      let first_cell_sticky_attr, default_sticky_attr =
        get_sticky_attrs ~top_sticky_pos
      in
      (List.mapi (Map.data columns) ~f:(fun i (key, data) ->
         let sticky_attr =
           if i = 0
           then [ first_cell_sticky_attr ]
           else [ default_sticky_attr    ]
         in
         let precedence_and_dir =
           Base_sort_criteria.find_precedence_and_dir sort_criteria key
         in
         let sort_direction_indicator =
           let indicator =
             Option.bind precedence_and_dir
               ~f:(fun (precedence, dir) -> Sort_dir.indicator dir ~precedence)
           in
           match indicator with
           | None     -> ""
           | Some ind -> sprintf " %s" ind
         in
         let sort_direction_classes =
           match precedence_and_dir with
           | None                   -> []
           | Some (precedence, dir) ->
             List.filter_opt
               [ Some "sorted"
               ; Sort_dir.class_ dir ~precedence
               ]
         in
         let on_click =
           Option.value_map
             (Column.sort_by data)
             ~default:[]
             ~f:(fun _ ->
               [ Attr.on_click (fun ev ->
                   match override_on_click with
                   | Some on_click -> on_click key ev
                   | None          -> inject (Action.Sort_column_clicked key)
                 )
               ]
             )
         in
         let attrs =
           [ Attr.id (Html_id.column_header_cell id key)
           ; Attr.classes ("column-header" :: sort_direction_classes)
           ]
           @ on_click
           @ sticky_attr
         in
         Node.th attrs [ data.Column.header; Node.text sort_direction_indicator]))
    in
    let group_nodes =
      let top_sticky_pos = finalize_sticky_pos top_sticky_pos in
      let first_cell_sticky_attr, default_sticky_attr =
        get_sticky_attrs ~top_sticky_pos
      in
      let%map columns = columns in
      let groups = List.map (Map.data columns) ~f:(fun c -> (snd c).Column.group) in
      if List.for_all groups ~f:Option.is_none
      then None
      else begin
        let grouped =
          List.groupi groups ~break:(fun i x y ->
            (i = 1 && Option.is_some left_sticky_pos)
            || not (Option.equal String.equal x y)
          )
        in
        List.mapi grouped ~f:(fun i l ->
          let sticky_attr =
            if i = 0
            then [ first_cell_sticky_attr ]
            else [ default_sticky_attr    ]
          in
          let text, class_ =
            match Option.join (List.hd l) with
            | None   -> ( [], "column-group-empty" )
            | Some s -> ( [Node.text s], "column-group-full" )
          in
          Node.th
            ([ Attr.classes [ "column-group"; class_ ]
             ; Attr.create "colspan" (Int.to_string (List.length l))
             ]
             @ sticky_attr
            )
            text
        )
        |> Option.some
      end
    in
    let group_attrs =
      let%map column_group_html_id = m >>| Model.column_group_html_id in
      [ Attr.id column_group_html_id ]
    in
    let header_attrs =
      let%map column_header_html_id = m >>| Model.column_header_html_id in
      [ Attr.id column_header_html_id ]
    in
    let%map group_nodes  = group_nodes
    and     header_nodes = header_nodes
    and     group_attrs  = group_attrs
    and     header_attrs = header_attrs
    in
    [ Option.map group_nodes ~f:(fun n -> Node.tr group_attrs n)
    ; Some (Node.tr header_attrs header_nodes)
    ]
    |> List.filter_opt

  type row_html_ids =
    { row_html_id : Html_id.t
    ; cell_html_ids : Html_id.t list
    }

  let view_rendered_rows ~table_id ~column_ids ~row_view ~render_row ~left_sticky_pos =
    let non_sticky_style = sticky_style ~sticky_pos:[] ~z_index:1 in
    let sticky_style =
      sticky_style ~sticky_pos:(Option.to_list left_sticky_pos) ~z_index:2
    in
    let%bind column_ids = column_ids in
    let column_id_strs = List.map column_ids ~f:Column_id.to_string in
    (* Annotate each row with its html ids - we do this because the string conversions can
       be expensive, and don't need to be re-done every time a row's incremental fires. *)
    let rows_to_render_with_html_ids =
      Incr.Map.unordered_fold (row_view >>| Row_view.rows_to_render)
        ~init:Key.Map.empty
        ~add:(fun ~key ~data:row acc ->
          let row_html_id = Html_id.row table_id key.row_id in
          let cell_html_ids =
            List.map column_id_strs ~f:(Html_id.cell_of_parts row_html_id)
          in
          Map.set acc ~key ~data:({ row_html_id; cell_html_ids }, row)
        )
        ~remove:(fun ~key ~data:_ acc -> Map.remove acc key)
        ~update:(fun ~key ~old_data:_ ~new_data:row acc ->
          Map.change acc key ~f:(Option.map ~f:(Tuple2.map_snd ~f:(fun _ -> row)))
        )
    in
    Incr.Map.mapi' rows_to_render_with_html_ids
      ~f:(fun ~key ~data ->
        let%bind { row_html_id; cell_html_ids } = data >>| fst in
        let%map { Row_node_spec. row_attrs; cells } =
          render_row ~row_id:key.row_id ~row:(data >>| snd)
        in
        let cells =
          List.zip_exn cell_html_ids cells
          |> List.mapi ~f:(fun i (cell_html_id, { Row_node_spec.Cell. attrs; node }) ->
            let sticky_style = if i = 0 then sticky_style else non_sticky_style in
            let attrs =
              attrs.other_attrs
              @ [ Attr.style (attrs.style @ sticky_style)
                ; Attr.id cell_html_id
                ]
            in
            Node.td attrs [ node ]
          )
        in
        let row_attrs = Row_node_spec.Attrs.to_vdom_attrs row_attrs in
        Node.tr ~key:row_html_id (row_attrs @ [ Attr.id row_html_id ]) cells
      )

  let view ?override_header_on_click m d ~render_row ~inject ~attrs =
    let spacer_before = unstage (spacer ~key:"before") in
    let spacer_after  = unstage (spacer ~key:"after")  in
    let columns = d >>| Derived_model.columns in
    let column_ids =
      let%map column_ids = d >>| Derived_model.columns in
      List.map (Map.data column_ids) ~f:fst
    in
    let row_view = d >>| Derived_model.row_view in
    let%bind table_id = m >>| Model.id
    and      top_sticky_pos  = sticky_pos "top"  (m >>| Model.float_header)
    and      left_sticky_pos = sticky_pos "left" (m >>| Model.float_first_col)
    in
    let left_sticky_pos = finalize_sticky_pos left_sticky_pos in
    let%map header =
      view_header ?override_on_click:override_header_on_click ~inject ~columns
        ~top_sticky_pos ~left_sticky_pos m
    and rendered_rows =
      view_rendered_rows ~table_id ~column_ids ~row_view ~render_row ~left_sticky_pos
    and before_height, after_height = Row_view.spacer_heights row_view
    in
    Node.table attrs
      [ Node.thead
          [ Attr.id (Html_id.thead table_id)
          ; Attr.style ([ "background-color", "inherit" ])
          ]
          header
      ; Node.tbody [Attr.id (Html_id.tbody table_id)]
          (spacer_before before_height
           @ Map.data rendered_rows
           @ spacer_after after_height)
      ]
  ;;
end

module Default_sort_spec = struct
  module Sort_key = struct
    type t =
      | String of string
      | Float of float
      | Integer of Int63.t
      | Null
    [@@deriving compare, sexp]
  end

  module Sort_dir = struct
    type t = Ascending | Descending [@@deriving sexp, compare]

    let next = function
      | None            -> Some Ascending
      | Some Ascending  -> Some Descending
      | Some Descending -> None

    let indicator t ~precedence =
      let dir_ind =
        match t with
        | Ascending -> "▲"
        | Descending -> "▼"
      in
      let precedence_ind =
        match precedence with
        | 1 -> ""
        | p -> sprintf "(%d)" p
      in
      Some (dir_ind ^ precedence_ind)

    let class_ t ~precedence:_ =
      match t with
      | Ascending  -> Some "sorted-asc"
      | Descending -> Some "sorted-desc"

    let sign = function
      | Ascending  -> 1
      | Descending -> -1
  end

  let compare_keys dir k1 k2 =
    match (k1:Sort_key.t), (k2:Sort_key.t) with
    (** Always sort nulls last regardless of the sort direction *)
    | Null, _ | _, Null -> Sort_key.compare k1 k2
    | _, _              -> Sort_key.compare k1 k2 * Sort_dir.sign dir

  let compare_rows_if_equal_keys ~cmp_row_id dir r1 r2 =
    cmp_row_id r1 r2 * Sort_dir.sign dir
end
