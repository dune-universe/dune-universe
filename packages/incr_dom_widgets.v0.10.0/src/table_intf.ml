open! Core_kernel
open! Import

(** [Sort_key] determines what values the rows are sorted on. *)
module type Sort_key = sig
  (** [t] is the type of the keys used to sort rows. Each row maps to a key of type [t]
      based on the contents of the row and the current column being sorted on. *)
  type t [@@deriving sexp, compare]
end

(** [Sort_dir] determines the different ways in which the rows can be sorted
    (e.g. ascending and descending). *)
module type Sort_dir = sig
  type t [@@deriving sexp, compare]

  (** [next] cycles through sort directions. This is used to determine how to
      update the sort direction when a header is clicked on. *)
  val next : t option -> t option

  (** [indicator] and [class_] convert the sort direction and precedence of a column from
      the sort criteria into a string symbol and a css class respectively in order to
      display sort information in the table.

      [indicator] returns a symbol that is displayed in the header of the corresponding
      column, while [class_] returns a css class that is assigned to the header element.

      The [precedence] is always a positive integer (i.e. it starts at 1, not 0).

      A column that is not in the sort criteria is assigned an [indicator] and [class_] of
      [None].

      Examples of suitable indicators are:
      - "▲"    (ascending with priority 1)
      - "▲(2)" (ascending with priority 2)
  *)

  val indicator : t -> precedence:int -> string option
  val class_    : t -> precedence:int -> string option
end

(** [Sort_spec] defines how rows are sorted in the table. *)
module type Sort_spec = sig
  module Sort_key : Sort_key
  module Sort_dir : Sort_dir

  (** [compare_keys] and [compare_rows_if_equal_keys] are comparison functions used to
      compare rows in a table in order to sort them.

      [compare_keys] compares two rows based on a single column in the table's sort
      criteria.

      [compare_rows_if_equal_keys] compares two rows based on their row ids, and is only
      called if the calls to [compare_key] for all columns in the table's sort criteria
      return 0. The sort direction is determined by the first column in the sort criteria.
  *)

  val compare_keys
    :  Sort_dir.t
    -> Sort_key.t
    -> Sort_key.t
    -> int

  val compare_rows_if_equal_keys
    :  cmp_row_id:('row_id -> 'row_id -> int)
    -> Sort_dir.t
    -> 'row_id
    -> 'row_id
    -> int
end

include Util

module type Id = sig
  type t [@@deriving sexp]
  include Comparable.S with type t := t

  val to_string : t -> string
end

module type S = sig
  module Row_id : Id

  (** This isn't just an int so that if an app adds columns the sort is maintained
      properly by the app's own classification instead of index. But it can be an int.*)
  module Column_id : Id

  module Sort_spec : Sort_spec

  module Sort_key = Sort_spec.Sort_key
  module Sort_dir = Sort_spec.Sort_dir

  (** A ['a Sort_criteria.t] specifies a list of columns by which to sort the rows of a
      table, in order of precedence from highest to lowest. Each column's sort criteria is
      made up of a direction and a value of type ['a], which varies as the sort criteria
      goes through several rounds of processing. *)
  module Sort_criteria : sig
    module By_column : sig
      type 'a t =
        { column : 'a
        ; dir    : Sort_dir.t
        }
      [@@deriving fields, compare, sexp]
    end

    type 'a t = 'a By_column.t list [@@deriving compare, sexp]
  end

  module Base_sort_criteria : sig
    type t = Column_id.t Sort_criteria.By_column.t list [@@deriving compare, sexp]

    val none : t
  end

  module Html_id : sig
    (** HTML element ids *)
    val table              : Table_id.t ->                            string
    val tbody              : Table_id.t ->                            string
    val thead              : Table_id.t ->                            string
    val column_group       : Table_id.t ->                            string
    val column_header      : Table_id.t ->                            string
    val column_header_cell : Table_id.t ->             Column_id.t -> string
    val row                : Table_id.t -> Row_id.t ->                string
    val cell               : Table_id.t -> Row_id.t -> Column_id.t -> string
  end

  module Column : sig
    type 'a t

    val create
      (** optionally render a row above the headers with their group names (similar to
          catalog). columns with the same group must be adjacent to be grouped together *)
      :  ?group:string
      (** used to extract a sortable value for this column from a row. *)
      -> ?sort_by:('a -> Sort_key.t)
      (** rendered at the top of the column.
          this node is wrapped in a <th> node with other attributes *)
      -> header:Vdom.Node.t
      -> unit
      -> 'a t

    val group : _ t -> string option
    val sort_by : 'a t -> ('a -> Sort_key.t) option
    val header : _ t -> Vdom.Node.t
  end

  (** This is the key used for sorting functionality. Apps don't need to touch this to use
      this widget, it is only exposed in case apps need to do something involving sorting
      in the same way as the table will. *)
  module Key : sig
    type t [@@deriving sexp]

    val sort_criteria : t -> Sort_key.t option Lazy.t Sort_criteria.t
    val sort_keys     : t -> Sort_key.t option Lazy.t list
    val sort_dirs     : t -> Sort_dir.t list
    val row_id        : t -> Row_id.t

    include Comparable.S with type t := t

    val create
      :  Sort_key.t option Lazy.t Sort_criteria.t
      -> Row_id.t
      -> t

    (** Sorts a map of rows in the same way as using the table's built in sort *)
    val sort
      :  'a Column.t Sort_criteria.t
      -> rows:'a Row_id.Map.t Incr.t
      -> 'a Map.t Incr.t
  end

  module Model : sig
    type t [@@deriving compare, sexp_of]

    val create
      (** How far scroll_to and focus moving should keep the row from the [scroll_region] edge *)
      :  scroll_margin:Margin.t
      (** Element to scroll in scroll_to and focus moves *)
      -> scroll_region:Scroll_region.Id.t
      (** Whether to float the table header fixed to the top or to a specified position on
          scrolling *)
      -> float_header:Float_type.t
      -> float_first_col:Float_type.t
      (** Estimated height of a normal row *)
      -> height_guess:float
      (** Id of the table.  This must be a fresh id - one that has not been passed to
          [Model.create] before - or behavior is undefined.  It maybe be useful to provide
          your own id here if you need access to the id before you create its associated
          [Model.t]. *)
      -> ?id:Table_id.t
      (** The column and sort direction that the table should be (initially) sorted by.
          Sorting can be changed later via clicking on column headers. If [initial_sort]
          is not specified, then the table is sorted by [Row_id]. *)
      -> ?initial_sort:Base_sort_criteria.t
      -> ?initial_focus_row:Row_id.t
      -> ?initial_focus_col:Column_id.t
      -> unit
      -> t

    val id : t -> Table_id.t

    val focus_row : t -> Row_id.t option
    val focus_col : t -> Column_id.t option

    val sort_criteria : t -> Base_sort_criteria.t
    val sort_columns  : t -> Column_id.t list
    val sort_dirs     : t -> Sort_dir.t list

    val scroll_margin : t -> Margin.t

    val set_sort_criteria : t -> Base_sort_criteria.t -> t

    val set_float_header : t -> Float_type.t -> t

    (** [cycle_sorting] computes and sets new sort criteria based on the current criteria.
        If the given column id already exists in the sort criteria, the column's sort
        direction is updated by calling [next_dir] on its current sort direction.
        Otherwise, the sort direction is computed by calling [next_dir] on [None]. If the
        new direction is [None], the column id is removed from the sort criteria.

        If [keep_existing_cols] is passed in as an argument, all existing column ids are
        kept in the sort criteria, and the given column id is added or promoted to the
        front of the sort criteria list (i.e. given the highest precedence).

        If [keep_existing_cols] is not passed in, all column ids apart from the given one
        are removed from the sort criteria.
    *)

    val cycle_sorting
      :  ?keep_existing_cols : unit
      -> t
      -> Column_id.t
      -> next_dir            : (Sort_dir.t option -> Sort_dir.t option)
      -> t

    (** Returns the bounding client rectangle of the table body. *)
    val get_tbody_rect : t -> float Js_misc.Rect.t option
  end

  module Derived_model : sig
    type 'a t

    val create
      :  Model.t Incr.t
      -> rows:'a Row_id.Map.t Incr.t
      (** This is a list and not a map so the app can decide order *)
      -> columns:(Column_id.t * 'a Column.t) list Incr.t
      -> 'a t Incr.t

    val sorted_rows   : 'a t -> 'a Key.Map.t
    val sort_criteria : 'a t -> 'a Column.t Sort_criteria.t
    val scroll_region : _ t -> Scroll_region.t option
  end

  module Action : sig
    type t [@@deriving sexp, compare]

    (** Moves the current focus in a given direction. If there is no focus it focuses the
        top or bottom row on moving down and up respectively. *)
    val move_focus_row : Focus_dir.t -> t
    val move_focus_col : Focus_dir.t -> t

    val set_focus_row : Row_id.t option -> t
    val set_focus_col : Column_id.t option -> t

    val page_focus_row : Focus_dir.t -> t
  end

  (** [current_key d row_id] returns [row_id]'s [Key.t] associated with the current sort
      criteria of [d]. Returns [None] if [row_id] does not exist in [d] *)
  val current_key
    :  _ Derived_model.t
    -> row_id:Row_id.t
    -> Key.t option

  (** Functions [scroll_*_into_scroll_region] and [scroll_*_to_position] will always work if
      the row heights in the model are correct (either because the row height estimate is
      correct for all rows, or because all rows have already been rendered and measured).
      If the row heights in the model are off, it may take multiple iterations of calling
      the scroll function and then remeasuring row heights in [update_visibility] before
      the specified element is successfully scrolled to its target. *)

  val scroll_row_into_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> Scroll_result.t

  val scroll_col_into_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> Scroll_result.t

  val scroll_focus_into_scroll_region
    :  Model.t
    -> _ Derived_model.t
    -> Scroll_result.t

  val scroll_row_to_position
    :  ?keep_in_scroll_region:unit
    -> Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> position:float
    -> Scroll_result.t

  val scroll_col_to_position
    :  ?keep_in_scroll_region:unit
    -> Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> position:float
    -> Scroll_result.t

  val scroll_focus_to_position
    :  ?keep_in_scroll_region:unit
    -> Model.t
    -> _ Derived_model.t
    -> position:(float * float)
    -> Scroll_result.t

  (** Functions [*_is_in_scroll_region] and [get_*_position] return [None] if the
      specified element is not found (e.g. there is no focus, or there is no row/column
      with the given id), or if the visibility measurements are not yet available.

      By default, the model's scroll margin is used to compute the bounds of the scroll
      region. However, if a [scroll_margin] argument is given, that will be use instead.
  *)

  val row_is_in_scroll_region
    :  ?scroll_margin:Margin.t
    -> Model.t
    -> _ Derived_model.t
    -> Row_id.t
    -> bool option

  val col_is_in_scroll_region
    :  ?scroll_margin:Margin.t
    -> Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> bool option

  val focus_is_in_scroll_region
    :  ?scroll_margin:Margin.t
    -> Model.t
    -> _ Derived_model.t
    -> bool option

  val get_row_position
    :  _ Derived_model.t
    -> Row_id.t
    -> float option

  val get_col_position
    :  Model.t
    -> _ Derived_model.t
    -> Column_id.t
    -> float option

  val get_focus_position
    :  Model.t
    -> _ Derived_model.t
    -> float option * float option

  (** Returns the bounding client rectangle of the currently focused cell, if any.
      This only returns a value if both the focus row and focus column are set. *)
  val get_focus_rect
    :  Model.t
    -> _ Derived_model.t
    -> float Js_misc.Rect.t option

  (** Finds the row id at a given vertical position on the page, or indicates that the
      position is before/after all the rows in the table.
      It only returns [None] if the model has no visibility info. *)
  val find_row_by_position
    :  Model.t
    -> _ Derived_model.t
    -> float
    -> [ `Before | `At of Row_id.t | `After ] option

  (** Finds the column id at a given horizontal position on the page, or indicates that
      the position is before/after all the columns in the table.
      It only returns [None] if the model has no visibility info or if a call to
      [Dom_html.getElementById_opt] on a header cell id returns [None]. *)
  val find_col_by_position
    :  Model.t
    -> _ Derived_model.t
    -> float
    -> [ `Before | `At of Column_id.t | `After ] option

  (** Used for scrolling to rows/columns upon focusing them *)
  val on_display
    :  old:Model.t
    -> Model.t
    -> _ Derived_model.t
    -> unit

  (** Used to handle sort column clicking *)
  val apply_action : Model.t -> _ Derived_model.t -> Action.t -> Model.t

  val set_focus_row : Model.t -> Row_id.t option -> Model.t
  val set_focus_col : Model.t -> Column_id.t option -> Model.t

  (** Measures rows, table and viewport *)
  val update_visibility : Model.t -> _ Derived_model.t -> Model.t

  (** When constructing the row [Vdom.Node.t] (most likely using function [Vdom.Node.tr]),
      it is important to pass in the argument [~key:id]. Otherwise scrolling may have
      unexpected behavior. *)
  type 'a row_renderer
    =  row_id:Row_id.t
    -> row:'a Incr.t
    -> Row_node_spec.t Incr.t

  (** Returns a full partially-rendered <table> node with header. [render_row] function
      should render <tr> nodes. *)
  val view
    :  ?override_header_on_click:(Column_id.t -> Dom_html.mouseEvent Js.t -> Vdom.Event.t)
    -> Model.t Incr.t
    -> 'a Derived_model.t Incr.t
    -> render_row:'a row_renderer
    -> inject:(Action.t -> Vdom.Event.t)
    -> attrs:Vdom.Attr.t list
    -> Vdom.Node.t Incr.t
end

module type Table = sig
  (** The Table widget is used to create incremental partially-rendered tables. It
      provides sorting, focus and (possibly floating) table header support.

      Behind the scenes it uses [Partial_render_list] so it works with very large tables
      (e.g 10,000 rows). *)

  module type S = S
  module type Id = Id
  module Focus_dir = Focus_dir
  module Margin = Margin
  module Scroll_region = Scroll_region
  module Float_type = Float_type
  module Make (Row_id : Id) (Column_id : Id) (Sort_spec : Sort_spec)
    : (S with module Row_id = Row_id
          and module Column_id = Column_id
          and module Sort_spec = Sort_spec)
end
