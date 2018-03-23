open! Core_kernel
open! Import

module Config = struct
  type t =
    { scroll_margin    : Util.Margin.t
    ; float_header     : Util.Float_type.t
    ; float_first_col  : Util.Float_type.t
    ; height_guess     : int
    ; initial_sort     : (Mesa_column.Id.t * Table.Default_sort_spec.Sort_dir.t) option
    ; classes          : string list
    ; styles           : Css.t
    } [@@deriving sexp_of, fields]

  let create
        ~scroll_margin ~float_header ~float_first_col ~height_guess
        ?initial_sort ?(classes=[]) ?(styles=Css.empty)
        ()
    =
    { scroll_margin
    ; float_header
    ; float_first_col
    ; height_guess
    ; initial_sort
    ; classes
    ; styles
    }
end

module type State = sig type t end

module type Row = sig
  module State : State

  module Id : sig
    type t [@@deriving sexp]

    include Comparable.S with type t := t
    include Stringable.S with type t := t
  end

  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  module Action : sig
    type row = t
    type t [@@deriving sexp_of]

    (** Used to construct a [t] to commit enqueued edits *)
    val commit_edit : Id.t -> row -> t

    val should_log : t -> bool
  end

  type action_error

  (** returns a function for updating the current map of rows in the table *)
  val apply_action
    :  Action.t
    -> State.t
    -> report_error:(action_error -> unit Async_kernel.Deferred.t)
    -> (t Id.Map.t -> t Id.Map.t Or_error.t) Staged.t

  val key_handler
    :  focus:(Id.t * t) option
    -> inject:(Action.t -> Vdom.Event.t)
    -> Dom_html.keyboardEvent Js.t
    -> Vdom.Event.t
end

module type S = sig
  module State : State
  module Row : Row
    with module State := State

  module Mode : sig
    module Search : sig
      type t

      val create : focus:[ `Row of Row.Id.t | `Scroll_top of int ] -> t

      val original_focus : t -> [ `Row of Row.Id.t | `Scroll_top of int ]
      val current_focus : t -> Row.Id.t option
    end

    type t =
      | View
      | Search of Search.t
      | Edit of string Mesa_cell.Id.Map.t
    [@@deriving compare]
  end

  module Table_widget
    : (Table.S
       with type Row_id.t = Row.Id.t
        and type Column_id.t = Mesa_column.Id.t)

  module Model : sig
    type t

    val create
      :  columns : Row.t Mesa_column.t Mesa_column.Id.Map.t
      -> Config.t
      -> t

    val cutoff : t -> t -> bool

    val mode : t -> Mode.t
    val rows : t -> Row.t Row.Id.Map.t
    val focus_row : t -> (Row.Id.t * Row.t) option
    val focus_col : t -> (Mesa_column.Id.t * Row.t Mesa_column.t) option
  end

  module Derived_model : sig
    type t

    val create : Model.t Incr.t -> t Incr.t
  end

  module Model_summary : sig
    type t = Model.t

    val create : Model.t -> Derived_model.t -> Model.t
  end

  module Action : sig
    type t [@@deriving sexp_of]

    val should_log : t -> bool

    val table_action : Table_widget.Action.t -> t
    val row_action   : Row.Action.t          -> t

    val move_focus_row : Util.Focus_dir.t -> t
    val set_focus_row  : Row.Id.t option  -> t
  end

  val key_handler
    :  Mode.t
    -> focus:(Row.Id.t * Row.t) option
    -> inject:(Action.t -> Vdom.Event.t)
    -> Dom_html.keyboardEvent Js.t
    -> Vdom.Event.t

  val apply_action
    :  Action.t
    -> Model.t
    -> State.t
    -> recompute_derived:(Model.t -> Derived_model.t)
    -> report_error:(Row.action_error -> unit Async_kernel.Deferred.t)
    -> Model.t Or_error.t

  val update_visibility
    :  Model.t
    -> Derived_model.t
    -> Model.t

  val on_display
    :  old:Model_summary.t
    -> Model.t
    -> Derived_model.t
    -> unit

  val view
    :  Model.t Incr.t
    -> Derived_model.t Incr.t
    -> inject:(Action.t -> Vdom.Event.t)
    -> attrs:Vdom.Attr.t list
    -> header:Vdom.Node.t
    -> Vdom.Node.t Incr.t
end

module type Mesa = sig

  (** Mesa provides an easy way of creating functional web tables. The user
      supplies a {e row} type and "actions" to be run on those {e rows} (along with some
      other funtionality) and the user gets back a table capable of:

      {ul
      {- Partial rendering}
      {- Navigating the table with the arrow keys and the mouse}
      {- Sorting the table on a column}
      {- Editing the table-cells' values and communicating those changes back to a server}
      {- Searching the table for a row}}

      Compared to table.mli, mesa are quicker and easier to set up but offer less
      flexibility.

      {2 Cell}

      Cells are the smallest building blocks of an mesa. To create a [Cell.t], users
      supply a [read] function as a way to project the cell value from {e row} and a
      [Cell.Kind.t] to determine how that value is displayed. Optionally, users can supply
      a [write] function as a way to inject values into {e row}.

      Multiple {e cells} can be displayed at the intersection of a {e row} and a {e
      column}. This is intended to enable users to group related data, e.g. buy/sell edge,
      min/max fair, etc.

      {2 Column}

      Columns define a group (of size one or more) of related data points within a single
      {e row}.  They accomplish this by housing one or more {e cells}, visually arranging
      them either horizontally or vertically. Each column has a header as well as an
      optional [group] to visually group multiple columns together. Users can also supply
      a [sort_by] function to enable sorting on that column.

      {2 Row}

      Rows are the individual data structures of the table, holding the actual data to be
      displayed in the table. In order to create the {e Mesa} module, the user must
      supply this type along with some additional functionality. An ID must be included as
      well as an [equal] function to determine when a row's data has changed, and
      therefore needs to be updated on the screen.

      {e Row} also requires {e Action}. {e Action} allows users to supply a set of events
      that can occur on a [Row.t]. One requirement for {e Action} is a way to construct an
      [Action.t] that signals that an edit has been commited. However, if none of your
      [Cell.t]s are writable, then this funtion will never be called.

      In addition, 2 functions are required; [apply_action] and
      [key_handler]. [apply_action] supplies a way to implement the [Action.t]s. The user
      is given their [report_error] function to report errors from a deferred
      operation. The [key_handler] tells {e Mesa} how to convert keyboard events into
      [Action.t]s and provides a funciton to turn those [Action.t]s into the necessary
      [Vdom.Event.t].

      {2 Mesa}

      [Make] returns a module that is easy to plug into your {e Incr_dom}
      program.

      {3 Mouse events}
      - Single clicks focuses a row. This is disabled when editing cells
      - Double clicks start edit mode for the row
      - Scroll wheel scrolls the table, however this is disabled when
        editing a row
        {3 Keyboard events}

      {v
      | Key code  | View mode               | Edit mode                                    | Search mode       |
      |-----------+-------------------------+----------------------------------------------+-------------------|
      | up/C-p    | move focus up one row   | move cursor to the next editable cell        |                   |
      | down/C-n  | move focus down one row | move cursor to the previous editable cell    |                   |
      | right/C-f |                         | move cursor to the next editable column      |                   |
      | left/C-b  |                         | move cursor to the previous editable column  |                   |
      | /         | start row search        |                                              |                   |
      | e         | start row edit          |                                              |                   |
      | Enter     | [Row.key_handler]       | commit edits                                 | focus current row |
      | Esc       | [Row.key_handler]       | abandon edits                                | abandon search    |
      | _         | [Row.key_handler]       | [Row.key_handler]                            | [Row.key_handler] |
      v}
  *)

  module type State = State
  module type Row = Row
  module type S = S
  module Config = Config
  module Make (State : State) (Row : Row with module State := State)
    : (S with module State = State
          and module Row = Row)

  type packed =
    | T : ((module S with type Model.t = 'model) * 'model) -> packed

  val pack
    :  (module S with type Model.t = 'model)
    -> 'model
    -> packed
end
