open Js_of_ocaml

module Indexable : sig
  type 'a t
  (** Indexable options also accept an array in which each item corresponds
      to the element at the same index. Note that this method requires
      to provide as many items as data, so, in most cases, using a function
      is more appropriated if supported. *)

  val of_single : 'a -> 'a t Js.t

  val of_js_array : 'a Js.js_array Js.t -> 'a t Js.t

  val of_array : 'a array -> 'a t Js.t

  val of_list : 'a list -> 'a t Js.t

  val cast_single : 'a t Js.t -> 'a Js.opt

  val cast_js_array : 'a t Js.t -> 'a Js.js_array Js.t Js.opt
end

module Scriptable : sig
  type ('a, 'b) t
  (** Scriptable options also accept a function which is called for each
      of the underlying data values and that takes the unique argument
      [context] representing contextual information. *)

  val of_fun : ('a -> 'b) -> ('a, 'b) t Js.t
end

module Scriptable_indexable : sig
  type ('a, 'b) t

  val of_single : 'b -> ('a, 'b) t Js.t

  val of_js_array : 'b Js.js_array Js.t -> ('a, 'b) t Js.t

  val of_array : 'b array -> ('a, 'b) t Js.t

  val of_list : 'b list -> ('a, 'b) t Js.t

  val of_fun : ('a -> 'b) -> ('a, 'b) t Js.t

  val cast_single : ('a, 'b) t Js.t -> 'b Js.opt

  val cast_js_array : ('a, 'b) t Js.t -> 'b Js.js_array Js.t Js.opt

  val cast_fun : ('a, 'b) t Js.t -> ('c, 'a -> 'b) Js.meth_callback Js.opt
end

module Line_cap : sig
  type t
  (** @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineCap> *)

  val butt : t Js.t

  val round : t Js.t

  val square : t Js.t

  val of_string : string -> t Js.t
end

module Line_join : sig
  type t
  (** @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineJoin> *)

  val round : t Js.t

  val bevel : t Js.t

  val miter : t Js.t

  val of_string : string -> t Js.t
end

module Interaction_mode : sig
  type t
  (** When configuring interaction with the graph via hover or tooltips,
      a number of different modes are available. *)

  val point : t Js.t
  (** Finds all of the items that intersect the point. *)

  val nearest : t Js.t
  (** Gets the items that are at the nearest distance to the point.
      The nearest item is determined based on the distance to the center
      of the chart item (point, bar). You can use the [axis] setting to define
      which directions are used in distance calculation. If [intersect] is [true],
      this is only triggered when the mouse position intersects an item
      in the graph. This is very useful for combo charts where points are hidden
      behind bars. *)

  val index : t Js.t
  (** Finds item at the same index.
      If the [intersect] setting is [true], the first intersecting item is used
      to determine the index in the data. If [intersect] is [false],
      the nearest item in the x direction is used to determine the index.
      To use index mode in a chart like the horizontal bar chart, where we search
      along the y direction, you can use the [axis] setting introduced in v2.7.0.
      By setting this value to ['y'] on the y direction is used. *)

  val dataset : t Js.t
  (** Finds items in the same dataset.
      If the [intersect] setting is [true], the first intersecting item is used
      to determine the index in the data.
      If [intersect] is [false], the nearest item is used to determine the index. *)

  val x : t Js.t
  (** Returns all items that would intersect based on the [X] coordinate
      of the position only. Would be useful for a vertical cursor implementation.
      Note that this only applies to cartesian charts. *)

  val y : t Js.t
  (** Returns all items that would intersect based on the [Y] coordinate
      of the position. This would be useful for a horizontal cursor implementation.
      Note that this only applies to cartesian charts. *)

  val of_string : string -> t Js.t
end

module Point_style : sig
  type t

  val circle : t Js.t

  val cross : t Js.t

  val crossRot : t Js.t

  val dash : t Js.t

  val line : t Js.t

  val rect : t Js.t

  val rectRounded : t Js.t

  val rectRot : t Js.t

  val star : t Js.t

  val triangle : t Js.t

  val of_string : string -> t Js.t

  val of_image : Dom_html.imageElement Js.t -> t Js.t

  val of_video : Dom_html.videoElement Js.t -> t Js.t

  val of_canvas : Dom_html.canvasElement Js.t -> t Js.t

  val cast_string : t Js.t -> string Js.opt

  val cast_image : t Js.t -> Dom_html.imageElement Js.t Js.opt

  val cast_video : t Js.t -> Dom_html.videoElement Js.t Js.opt

  val cast_canvas : t Js.t -> Dom_html.canvasElement Js.t Js.opt
end

module Easing : sig
  type t

  val linear : t Js.t

  val easeInQuad : t Js.t

  val easeOutQuad : t Js.t

  val easeInOutQuad : t Js.t

  val easeInCubic : t Js.t

  val easeOutCubic : t Js.t

  val easeInOutCubic : t Js.t

  val easeInQuart : t Js.t

  val easeOutQuart : t Js.t

  val easeInOutQuart : t Js.t

  val easeInQuint : t Js.t

  val easeOutQuint : t Js.t

  val easeInOutQuint : t Js.t

  val easeInSine : t Js.t

  val easeOutSine : t Js.t

  val easeInOutSine : t Js.t

  val easeInExpo : t Js.t

  val easeOutExpo : t Js.t

  val easeInOutExpo : t Js.t

  val easeInCirc : t Js.t

  val easeOutCirc : t Js.t

  val easeInOutCirc : t Js.t

  val easeInElastic : t Js.t

  val easeOutElastic : t Js.t

  val easeInOutElastic : t Js.t

  val easeInBack : t Js.t

  val easeOutBack : t Js.t

  val easeInOutBack : t Js.t

  val easeInBounce : t Js.t

  val easeOutBounce : t Js.t

  val easeInOutBounce : t Js.t

  val of_string : string -> t Js.t
end

module Padding : sig
  type t
  (** If this value is a number, it is applied to all sides of the element
      (left, top, right, bottom). If this value is an object, the [left]
      property defines the left padding. Similarly the [right], [top] and
      [bottom] properties can also be specified. *)

  class type obj =
    object
      method top : int Js.optdef_prop

      method right : int Js.optdef_prop

      method bottom : int Js.optdef_prop

      method left : int Js.optdef_prop
    end

  val make_object : ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> t Js.t

  val of_object : obj Js.t -> t Js.t

  val of_int : int -> t Js.t

  val cast_int : t Js.t -> int Js.opt

  val cast_object : t Js.t -> obj Js.t Js.opt
end

module Color : sig
  type t
  (** When supplying colors to Chart options, you can use a number of formats.
      You can specify the color as a string in hexadecimal, RGB, or HSL notations.
      If a color is needed, but not specified, Chart.js will use the global
      default color. This color is stored at [Chart.defaults.global.defaultColor].
      It is initially set to ['rgba(0, 0, 0, 0.1)'].
      You can also pass a [CanvasGradient] object.
      You will need to create this before passing to the chart,
      but using it you can achieve some interesting effects. *)

  val of_string : string -> t Js.t

  val of_canvas_gradient : Dom_html.canvasGradient Js.t -> t Js.t

  val of_canvas_pattern : Dom_html.canvasPattern Js.t -> t Js.t

  val cast_string : t Js.t -> string Js.opt

  val cast_canvas_gradient : t Js.t -> Dom_html.canvasGradient Js.t Js.opt

  val cast_canvas_pattern : t Js.t -> Dom_html.canvasPattern Js.t Js.opt
end

module Position : sig
  type t

  val left : t Js.t

  val right : t Js.t

  val top : t Js.t

  val bottom : t Js.t

  val of_string : string -> t Js.t
end

module Tooltip_position : sig
  type t

  val average : t
  (** Will place the tooltip at the average position
      of the items displayed in the tooltip. *)

  val nearest : t
  (** Will place the tooltip at the position of the element
      closest to the event position. *)

  val of_string : string -> t
end

module Line_height : sig
  type t
  (** @see <https://developer.mozilla.org/en-US/docs/Web/CSS/line-height> *)

  val of_string : string -> t Js.t

  val of_float : float -> t Js.t

  val cast_string : t Js.t -> string Js.opt

  val cast_float : t Js.t -> float Js.opt
end

module Hover_axis : sig
  type t

  val x : t Js.t

  val y : t Js.t

  val xy : t Js.t

  val of_string : string -> t Js.t
end

module Fill : sig
  type t

  val zero : t Js.t

  val top : t Js.t

  val bottom : t Js.t

  val _true : t Js.t

  val _false : t Js.t

  val of_bool : bool -> t Js.t

  val of_string : string -> t Js.t

  val cast_bool : t Js.t -> bool Js.opt

  val cast_string : t Js.t -> string Js.opt
end

module Time : sig
  type t

  val of_float_s : float -> t Js.t

  val of_int_s : int -> t Js.t

  val of_string : string -> t Js.t

  val of_array : int array -> t Js.t

  val of_js_array : int Js.js_array Js.t -> t Js.t

  val of_date : Js.date Js.t -> t Js.t

  val cast_float_s : t Js.t -> float Js.opt

  val cast_string : t Js.t -> string Js.opt

  val cast_js_array : t Js.t -> int Js.js_array Js.t Js.opt

  val cast_date : t Js.t -> Js.date Js.t Js.opt
end

module Or_false : sig
  type 'a t

  val make : 'a -> 'a t Js.t

  val _false : 'a t Js.t
end

type line_dash = float Js.js_array Js.t
(** @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash> *)

type line_dash_offset = float
(** @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset> *)

module Time_ticks_source : sig
  type t

  val auto : t Js.t

  val data : t Js.t

  val labels : t Js.t

  val of_string : string -> t Js.t
end

module Time_distribution : sig
  type t

  val linear : t Js.t
  (** Data points are spread according to their time (distances can vary). *)

  val series : t Js.t
  (** Data points are spread at the same distance from each other. *)

  val of_string : string -> t Js.t
end

module Time_bounds : sig
  type t

  val data : t Js.t
  (** Makes sure data are fully visible, labels outside are removed. *)

  val ticks : t Js.t
  (** Makes sure ticks are fully visible, data outside are truncated. *)

  val of_string : string -> t Js.t
end

module Time_unit : sig
  type t

  val millisecond : t Js.t

  val second : t Js.t

  val minute : t Js.t

  val hour : t Js.t

  val day : t Js.t

  val week : t Js.t

  val month : t Js.t

  val quarter : t Js.t

  val year : t Js.t

  val of_string : string -> t Js.t
end

module Interpolation_mode : sig
  type t

  val default : t Js.t
  (** Default algorithm uses a custom weighted cubic interpolation,
      which produces pleasant curves for all types of datasets. *)

  val monotone : t Js.t
  (** Monotone algorithm is more suited to [y = f(x)] datasets :
      it preserves monotonicity (or piecewise monotonicity) of the dataset
      being interpolated, and ensures local extremums (if any) stay at input
      data points. *)

  val of_string : string -> t Js.t
end

module Stepped_line : sig
  type t

  val _false : t Js.t
  (** No Step Interpolation. *)

  val _true : t Js.t
  (** Step-before Interpolation (same as [before]). *)

  val before : t Js.t
  (** Step-before Interpolation. *)

  val after : t Js.t
  (** Step-after Interpolation. *)

  val middle : t Js.t
  (** Step-middle Interpolation. *)

  val of_bool : bool -> t Js.t

  val of_string : string -> t Js.t
end

module Line_fill : sig
  type t

  val relative : int -> t Js.t

  val absolute : int -> t Js.t

  val _false : t Js.t

  val _true : t Js.t
  (** Equivalent to [origin] *)

  val start : t Js.t

  val _end : t Js.t

  val origin : t Js.t

  val of_bool : bool -> t Js.t

  val of_string : string -> t Js.t
end

module Pie_border_align : sig
  type t

  val center : t Js.t
  (** The borders of arcs next to each other will overlap. *)

  val inner : t Js.t
  (** Guarantees that all the borders do not overlap. *)
end

module Axis_display : sig
  type t

  val auto : t Js.t

  val cast_bool : t Js.t -> bool Js.opt

  val is_auto : t Js.t -> bool

  val of_bool : bool -> t Js.t

  val of_string : string -> t Js.t
end

module Time_parser : sig
  type t

  val of_string : string -> t Js.t
  (** A custom format to be used by Moment.js to parse the date. *)

  val of_fun : ('a -> 'b Js.t) -> t Js.t
  (** A function must return a Moment.js object given the appropriate data value. *)

  val cast_string : t Js.t -> string Js.opt

  val cast_fun : t Js.t -> ('a -> 'b Js.t) Js.callback Js.opt
end

module Bar_thickness : sig
  type t

  val flex : t Js.t

  val of_int : int -> t Js.t

  val of_float : float -> t Js.t

  val is_flex : t Js.t -> bool

  val cast_number : t Js.t -> Js.number Js.t Js.opt
end

type 'a tick_cb = ('a -> int -> 'a Js.js_array Js.t) Js.callback

type ('a, 'b, 'c) tooltip_cb =
  ('a, 'b -> 'c -> Js.js_string Js.t Indexable.t Js.t) Js.meth_callback Js.optdef

class type ['x, 'y] dataPoint =
  object
    method x : 'x Js.prop

    method y : 'y Js.prop
  end

class type ['t, 'y] dataPointT =
  object
    method t : 't Js.prop

    method y : 'y Js.prop
  end

class type ['x, 'y, 'r] dataPointR =
  object
    inherit ['x, 'y] dataPoint

    method r : 'r Js.prop
  end

val create_data_point : x:'a -> y:'b -> ('a, 'b) dataPoint Js.t

val create_data_point_t : t:'a -> y:'b -> ('a, 'b) dataPointT Js.t

val create_data_point_r : x:'a -> y:'b -> r:'c -> ('a, 'b, 'c) dataPointR Js.t

(** {1 Axes} *)

(** The minorTick configuration is nested under the ticks configuration
    in the [minor] key. It defines options for the minor tick marks that are
    generated by the axis. Omitted options are inherited from ticks
    configuration. *)
class type minorTicks =
  object
    (** Returns the string representation of the tick value
      as it should be displayed on the chart. *)
    method callback : 'a tick_cb Js.prop

    (** Font color for tick labels. *)
    method fontColor : Color.t Js.t Js.optdef_prop

    (** Font family for the tick labels, follows CSS font-family options. *)
    method fontFamily : Js.js_string Js.t Js.optdef_prop

    (** Font size for the tick labels. *)
    method fontSize : int Js.optdef_prop

    (** Font style for the tick labels, follows CSS font-style options
      (i.e. normal, italic, oblique, initial, inherit). *)
    method fontStyle : Js.js_string Js.t Js.optdef_prop
  end

and majorTicks = minorTicks
(** The majorTick configuration is nested under the ticks configuration
    in the [major] key. It defines options for the major tick marks that are
    generated by the axis. Omitted options are inherited from ticks configuration.
    These options are disabled by default. *)

(** The tick configuration is nested under the scale configuration
    in the ticks key. It defines options for the tick marks that are
    generated by the axis.*)
and ticks =
  object
    (** Returns the string representation of the tick value as
      it should be displayed on the chart. *)
    method callback : 'a tick_cb Js.prop

    (** If [true], show tick marks. *)
    method display : bool Js.t Js.prop

    (** Font color for tick labels. *)
    method fontColor : Color.t Js.t Js.optdef_prop

    (** Font family for the tick labels, follows CSS font-family options. *)
    method fontFamily : Js.js_string Js.t Js.optdef_prop

    (** Font size for the tick labels. *)
    method fontSize : int Js.optdef_prop

    (** Font style for the tick labels, follows CSS font-style options
      (i.e. normal, italic, oblique, initial, inherit). *)
    method fontStyle : Js.js_string Js.t Js.optdef_prop

    (** Reverses order of tick labels. *)
    method reverse : bool Js.t Js.prop

    (** Minor ticks configuration. Omitted options are inherited
      from options above. *)
    method minor : minorTicks Js.t

    (** Major ticks configuration. Omitted options are inherited
      from options above.*)
    method major : majorTicks Js.t
  end

and scaleLabel =
  object
    (** If true, display the axis title. *)
    method display : bool Js.t Js.prop

    (** The text for the title. (i.e. "# of People" or "Response Choices"). *)
    method labelString : Js.js_string Js.t Js.prop

    (** Height of an individual line of text. *)
    method lineHeight : Line_height.t Js.t Js.prop

    (** Font color for scale title. *)
    method fontColor : Color.t Js.t Js.prop

    (** Font family for the scale title, follows CSS font-family options. *)
    method fontFamily : Js.js_string Js.t Js.prop

    (** Font size for scale title. *)
    method fontSize : int Js.prop

    (** Font style for the scale title, follows CSS font-style options
      (i.e. normal, italic, oblique, initial, inherit) *)
    method fontStyle : Js.js_string Js.t Js.prop

    (** Padding to apply around scale labels.
      Only top and bottom are implemented. *)
    method padding : Padding.t Js.t Js.prop
  end

and gridLines =
  object
    (** If [false], do not display grid lines for this axis. *)
    method display : bool Js.t Js.prop

    (** If [true], gridlines are circular (on radar chart only). *)
    method circular : bool Js.t Js.prop

    (** The color of the grid lines. If specified as an array,
      the first color applies to the first grid line, the second
      to the second grid line and so on. *)
    method color : Color.t Js.t Indexable.t Js.t Js.prop

    (** Length and spacing of dashes on grid lines. *)
    method borderDash : line_dash Js.prop

    (** Offset for line dashes. *)
    method borderDashOffset : line_dash_offset Js.prop

    (** Stroke width of grid lines. *)
    method lineWidth : int Indexable.t Js.t Js.prop

    (** If true, draw border at the edge between the axis and the chart area. *)
    method drawBorder : bool Js.t Js.prop

    (** If true, draw lines on the chart area inside the axis lines.
      This is useful when there are multiple axes and you need to
      control which grid lines are drawn. *)
    method drawOnChartArea : bool Js.t Js.prop

    (** If true, draw lines beside the ticks in the axis area beside the chart. *)
    method drawTicks : bool Js.t Js.prop

    (** Length in pixels that the grid lines will draw into the axis area. *)
    method tickMarkLength : int Js.prop

    (** Stroke width of the grid line for the first index (index 0). *)
    method zeroLineWidth : int Js.prop

    (** Stroke color of the grid line for the first index (index 0). *)
    method zeroLineColor : Color.t Js.t Js.prop

    (** Length and spacing of dashes of the grid line
      for the first index (index 0). *)
    method zeroLineBorderDash : line_dash Js.prop

    (** Offset for line dashes of the grid line for the first index (index 0). *)
    method zeroLineBorderDashOffset : line_dash_offset Js.prop

    (** If [true], grid lines will be shifted to be between labels.
      This is set to true for a category scale in a bar chart by default. *)
    method offsetGridLines : bool Js.t Js.prop
  end

class type axis =
  object
    (** Type of scale being employed
      Custom scales can be created and registered with a string key.
      This allows changing the type of an axis for a chart. *)
    method _type : Js.js_string Js.t Js.prop

    (** Controls the axis global visibility
      (visible when [true], hidden when [false]).
      When display is ['auto'], the axis is visible only
      if at least one associated dataset is visible. *)
    method display : Axis_display.t Js.t Js.prop

    (** The weight used to sort the axis.
      Higher weights are further away from the chart area. *)
    method weight : float Js.optdef_prop

    (** Callback called before the update process starts. *)
    method beforeUpdate : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs before dimensions are set. *)
    method beforeSetDimensions : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs after dimensions are set. *)
    method afterSetDimensions : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs before data limits are determined. *)
    method beforeDataLimits : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs after data limits are determined. *)
    method afterDataLimits : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs before ticks are created. *)
    method beforeBuildTicks : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs after ticks are created. Useful for filtering ticks.
      @return the filtered ticks. *)
    method afterBuildTicks :
      ('a Js.t -> 'tick Js.js_array Js.t -> 'tick Js.js_array Js.t) Js.callback
      Js.optdef_prop

    (** Callback that runs before ticks are converted into strings. *)
    method beforeTickToLabelConversion : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs after ticks are converted into strings. *)
    method afterTickToLabelConversion : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs before tick rotation is determined. *)
    method beforeCalculateTickRotation : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs after tick rotation is determined. *)
    method afterCalculateTickRotation : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs before the scale fits to the canvas. *)
    method beforeFit : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs after the scale fits to the canvas. *)
    method afterFit : ('a Js.t -> unit) Js.callback Js.optdef_prop

    (** Callback that runs at the end of the update process. *)
    method afterUpdate : ('a Js.t -> unit) Js.callback Js.optdef_prop
  end

val empty_minor_ticks : unit -> minorTicks Js.t

val empty_major_ticks : unit -> majorTicks Js.t

val empty_ticks : unit -> ticks Js.t

val empty_scale_label : unit -> scaleLabel Js.t

val empty_grid_lines : unit -> gridLines Js.t

(** {2 Cartesian axes} *)

class type cartesianTicks =
  object
    inherit ticks

    (** If [true], automatically calculates how many labels that
      can be shown and hides labels accordingly. Turn it off to show all
      labels no matter what. *)
    method autoSkip : bool Js.t Js.prop

    (** Padding between the ticks on the horizontal axis when autoSkip is
      enabled. Note: Only applicable to horizontal scales. *)
    method autoSkipPadding : int Js.prop

    (** Distance in pixels to offset the label from the centre point of the
      tick (in the y direction for the x axis, and the x direction for the
      y axis). Note: this can cause labels at the edges to be cropped by the
      edge of the canvas. *)
    method labelOffset : int Js.prop

    (** Maximum rotation for tick labels when rotating to condense labels.
      Note: Rotation doesn't occur until necessary.
      Note: Only applicable to horizontal scales. *)
    method maxRotation : int Js.prop

    (** Minimum rotation for tick labels.
      Note: Only applicable to horizontal scales. *)
    method minRotation : int Js.prop

    (** Flips tick labels around axis, displaying the labels inside the chart
      instead of outside. Note: Only applicable to vertical scales. *)
    method mirror : bool Js.prop

    (** Padding between the tick label and the axis. When set on a vertical axis,
      this applies in the horizontal (X) direction. When set on a horizontal
      axis, this applies in the vertical (Y) direction. *)
    method padding : int Js.prop
  end

class type cartesianAxis =
  object
    inherit axis

    (** Position of the axis in the chart.
      Possible values are: ['top'], ['left'], ['bottom'], ['right'] *)
    method position : Position.t Js.t Js.prop

    (** If [true], extra space is added to the both edges and the axis
      is scaled to fit into the chart area. This is set to true for a
      category scale in a bar chart by default. *)
    method offset : bool Js.t Js.prop

    (** The ID is used to link datasets and scale axes together. *)
    method id : Js.js_string Js.t Js.prop

    (** Grid line configuration. *)
    method gridLines : gridLines Js.t Js.prop

    (** Scale title configuration. *)
    method scaleLabel : scaleLabel Js.t Js.prop

    (** Ticks configuration. *)
    method _ticks : cartesianTicks Js.t Js.prop
  end

val coerce_cartesian_axis : #cartesianAxis Js.t -> cartesianAxis Js.t

val empty_cartesian_axis : unit -> cartesianAxis Js.t

(** {3 Category axis} *)

class type categoryTicks =
  object
    inherit cartesianTicks

    (** An array of labels to display. *)
    method labels : Js.js_string Js.t Js.optdef_prop

    (** The minimum item to display. *)
    method min : Js.js_string Js.t Js.optdef Js.prop

    (** The maximum item to display. *)
    method max : Js.js_string Js.t Js.optdef Js.prop
  end

class type categoryAxis =
  object
    inherit cartesianAxis

    method ticks : categoryTicks Js.t Js.prop
  end

val empty_category_ticks : unit -> categoryTicks Js.t

val empty_category_axis : unit -> categoryAxis Js.t

(** {3 Linear axis} *)

class type linearTicks =
  object
    inherit cartesianTicks

    (** If [true], scale will include 0 if it is not already included. *)
    method beginAtZero : bool Js.t Js.optdef_prop

    (** User defined minimum number for the scale,
      overrides minimum value from data. *)
    method min : float Js.optdef Js.prop

    (** User defined maximum number for the scale,
      overrides maximum value from data. *)
    method max : float Js.optdef Js.prop

    (** Maximum number of ticks and gridlines to show. *)
    method maxTicksLimit : int Js.prop

    (** If defined and stepSize is not specified,
      the step size will be rounded to this many decimal places. *)
    method precision : int Js.optdef_prop

    (** User defined fixed step size for the scale. *)
    method stepSize : int Js.optdef_prop

    (** Adjustment used when calculating the maximum data value. *)
    method suggestedMax : float Js.optdef Js.prop

    (** Adjustment used when calculating the minimum data value. *)
    method suggestedMin : float Js.optdef Js.prop
  end

class type linearAxis =
  object
    inherit cartesianAxis

    method ticks : linearTicks Js.t Js.prop
  end

val empty_linear_ticks : unit -> linearTicks Js.t

val empty_linear_axis : unit -> linearAxis Js.t

(** {3 Logarithmic axis} *)

class type logarithmicTicks =
  object
    inherit cartesianTicks

    (** User defined minimum number for the scale,
      overrides minimum value from data. *)
    method min : float Js.optdef Js.prop

    (** User defined maximum number for the scale,
      overrides maximum value from data. *)
    method max : float Js.optdef Js.prop
  end

class type logarithmicAxis =
  object
    inherit cartesianAxis

    method ticks : logarithmicTicks Js.t Js.prop
  end

val empty_logarithmic_ticks : unit -> logarithmicTicks Js.t

val empty_logarithmic_axis : unit -> logarithmicAxis Js.t

(** {3 Time axis} *)

(** The following display formats are used to configure
    how different time units are formed into strings for
    the axis tick marks. *)
class type timeDisplayFormats =
  object
    method millisecond : Js.js_string Js.t Js.prop

    method second : Js.js_string Js.t Js.prop

    method minute : Js.js_string Js.t Js.prop

    method hour : Js.js_string Js.t Js.prop

    method day : Js.js_string Js.t Js.prop

    method week : Js.js_string Js.t Js.prop

    method month : Js.js_string Js.t Js.prop

    method quarter : Js.js_string Js.t Js.prop

    method year : Js.js_string Js.t Js.prop
  end

class type timeTicks =
  object
    inherit cartesianTicks

    (** How ticks are generated.
      [auto]: generates "optimal" ticks based on scale size and time options
      [data]: generates ticks from data (including labels from data objects)
      [labels]: generates ticks from user given data.labels values ONLY *)
    method source : Time_ticks_source.t Js.t Js.prop
  end

class type timeOptions =
  object
    (** Sets how different time units are displayed. *)
    method displayFormats : timeDisplayFormats Js.t Js.optdef_prop

    (** If [true] and the unit is set to 'week', then the first day
      of the week will be Monday. Otherwise, it will be Sunday. *)
    method isoWeekday : bool Js.t Js.prop

    (** If defined, this will override the data maximum *)
    method max : Time.t Js.t Js.optdef_prop

    (** If defined, this will override the data minimum *)
    method min : Time.t Js.t Js.optdef_prop

    (** Custom parser for dates. *)
    method _parser : Time_parser.t Js.t Js.optdef_prop

    (** If defined, dates will be rounded to the start of this unit. *)
    method round : Time_unit.t Js.t Or_false.t Js.t Js.prop

    (** The moment js format string to use for the tooltip. *)
    method tooltipFormat : Js.js_string Js.t Js.optdef_prop

    (** If defined, will force the unit to be a certain type. *)
    method unit : Time_unit.t Js.t Or_false.t Js.t Js.prop

    (** The number of units between grid lines. *)
    method stepSize : int Js.prop

    (** The minimum display format to be used for a time unit. *)
    method minUnit : Time_unit.t Js.t Js.prop
  end

class type timeAxis =
  object
    inherit cartesianAxis

    method ticks : timeTicks Js.t Js.prop

    method time : timeOptions Js.t Js.prop

    (** The distribution property controls the data distribution along the scale:
      [linear]: data are spread according to their time (distances can vary)
      [series]: data are spread at the same distance from each other *)
    method distribution : Time_distribution.t Js.t Js.prop

    (** The bounds property controls the scale boundary strategy
      (bypassed by [min]/[max] time options).
      [data]: makes sure data are fully visible, labels outside are removed
      [ticks]: makes sure ticks are fully visible, data outside are truncated *)
    method bounds : Time_bounds.t Js.t Js.prop
  end

val empty_time_display_formats : unit -> timeDisplayFormats Js.t

val empty_time_ticks : unit -> timeTicks Js.t

val empty_time_options : unit -> timeOptions Js.t

val empty_time_axis : unit -> timeAxis Js.t

class type dataset =
  object
    method _type : Js.js_string Js.t Js.optdef_prop

    method label : Js.js_string Js.t Js.prop

    method hidden : bool Js.t Js.optdef_prop
  end

val coerce_dataset : #dataset Js.t -> dataset Js.t

class type data =
  object
    method datasets : #dataset Js.t Js.js_array Js.t Js.prop

    method labels : 'a Js.js_array Js.t Js.optdef_prop

    method xLabels : 'a Js.js_array Js.t Js.optdef_prop

    method yLabels : 'a Js.js_array Js.t Js.optdef_prop
  end

val empty_data : unit -> data Js.t

(** {1 Chart configuration} *)

class type updateConfig =
  object
    method duration : int Js.optdef_prop

    method _lazy : bool Js.t Js.optdef_prop

    method easing : Easing.t Js.optdef_prop
  end

(** {2 Animation} *)

class type animationItem =
  object
    (** Chart object. *)
    method chart : chart Js.t Js.readonly_prop

    (** Current Animation frame number. *)
    method currentStep : float Js.readonly_prop

    (** Number of animation frames. *)
    method numSteps : float Js.readonly_prop

    (** Function that renders the chart. *)
    method render : chart Js.t -> animationItem Js.t -> unit Js.meth

    (** User callback. *)
    method onAnimationProgress : animationItem Js.t -> unit Js.meth

    (** User callback. *)
    method onAnimationComplete : animationItem Js.t -> unit Js.meth
  end

and animation =
  object
    (** The number of milliseconds an animation takes. *)
    method duration : int Js.prop

    (** Easing function to use. *)
    method easing : Easing.t Js.prop

    (** Callback called on each step of an animation. *)
    method onProgress : (animationItem Js.t -> unit) Js.callback Js.opt Js.prop

    (** Callback called at the end of an animation. *)
    method onComplete : (animationItem Js.t -> unit) Js.callback Js.opt Js.prop
  end

(** {2 Layout} *)

and layout =
  object
    (** The padding to add inside the chart. *)
    method padding : Padding.t Js.prop
  end (* FIXME this interface differs between Pie and other chart types *)

(** {2 Legend} *)

and legendItem =
  object
    (** Label that will be displayed. *)
    method text : Js.js_string Js.t Js.prop
    (* FIXME seems it can be Indexable & Scriptable dependent on chart type *)

    (** Fill style of the legend box. *)
    method fillStyle : Color.t Js.t Js.prop

    (** If [true], this item represents a hidden dataset.
      Label will be rendered with a strike-through effect. *)
    method hidden : bool Js.t Js.prop

    (** For box border. *)
    method lineCap : Line_cap.t Js.t Js.optdef_prop

    (** For box border. *)
    method lineDash : line_dash Js.optdef_prop

    (** For box border. *)
    method lineDashOffset : line_dash_offset Js.optdef_prop

    (** For box border. *)
    method lineJoin : Line_join.t Js.t Js.optdef_prop

    (** Width of box border. *)
    method lineWidth : int Js.prop

    (** Stroke style of the legend box. *)
    method strokeStyle : Color.t Js.t Js.prop

    (** Point style of the legend box (only used if usePointStyle is true) *)
    method pointStyle : Point_style.t Js.t Js.optdef_prop

    method datasetIndex : int Js.prop
  end

and legendLabels =
  object ('self)
    (** Width of coloured box. *)
    method boxWidth : int Js.prop

    (** Font size of text. *)
    method fontSize : int Js.optdef_prop

    (** Font style of text. *)
    method fontStyle : Js.js_string Js.t Js.optdef_prop

    (** Color of text. *)
    method fontColor : Color.t Js.t Js.optdef_prop

    (** Font family of legend text. *)
    method fontFamily : Js.js_string Js.t Js.optdef_prop

    (** Padding between rows of colored boxes. *)
    method padding : int Js.prop

    (** Generates legend items for each thing in the legend.
      Default implementation returns the text + styling for the color box. *)
    method generateLabels :
      (chart Js.t -> legendItem Js.t Js.js_array Js.t) Js.callback Js.prop

    (** Filters legend items out of the legend. Receives 2 parameters,
      a Legend Item and the chart data. *)
    method filter :
      ('self, legendItem Js.t -> data Js.t -> bool Js.t) Js.meth_callback Js.optdef_prop

    (** Label style will match corresponding point style
      (size is based on fontSize, boxWidth is not used in this case). *)
    method usePointStyle : bool Js.t Js.optdef_prop
  end

and legend =
  object
    (** Is the legend shown. *)
    method display : bool Js.t Js.prop

    (** Position of the legend. *)
    method position : Position.t Js.t Js.prop

    (** Marks that this box should take the full width of the canvas
      (pushing down other boxes). This is unlikely to need to be changed
      in day-to-day use. *)
    method fullWidth : bool Js.t Js.prop

    (** A callback that is called when a click event is
      registered on a label item *)
    method onClick :
      (chart, Dom_html.event Js.t -> legendItem Js.t -> unit) Js.meth_callback
      Js.optdef_prop

    (** A callback that is called when a 'mousemove' event is
      registered on top of a label item *)
    method onHover :
      (chart, Dom_html.event Js.t -> legendItem Js.t -> unit) Js.meth_callback
      Js.optdef_prop

    method onLeave :
      (chart, Dom_html.event Js.t -> legendItem Js.t -> unit) Js.meth_callback
      Js.optdef_prop

    (** Legend will show datasets in reverse order. *)
    method reverse : bool Js.t Js.prop

    (** Legend label configuration. *)
    method labels : legendLabels Js.t Js.prop
  end

(** {2 Title} *)

and title =
  object
    (** Is the title shown. *)
    method display : bool Js.t Js.prop

    (** Position of title. *)
    method position : Position.t Js.t Js.prop

    (** Font size. *)
    method fontSize : int Js.optdef_prop

    (** Font family for the title text. *)
    method fontFamily : Js.js_string Js.t Js.optdef_prop

    (** Font color. *)
    method fontColor : Js.js_string Js.t Js.optdef_prop

    (** Font style. *)
    method fontStyle : Js.js_string Js.t Js.optdef_prop

    method fullWidth : bool Js.t Js.prop

    (** Number of pixels to add above and below the title text. *)
    method padding : int Js.prop

    (** Height of an individual line of text. *)
    method lineHeight : Line_height.t Js.t Js.optdef_prop

    (** Title text to display. If specified as an array,
      text is rendered on multiple lines. *)
    method text : Js.js_string Js.t Indexable.t Js.t Js.prop
  end

(** {2 Tooltip} *)

and tooltipItem =
  object
    (** Label for the tooltip. *)
    method label : Js.js_string Js.t Js.readonly_prop

    (** Value for the tooltip. *)
    method value : Js.js_string Js.t Js.readonly_prop

    (** Index of the dataset the item comes from. *)
    method datasetIndex : int Js.readonly_prop

    (** Index of this data item in the dataset. *)
    method index : int Js.readonly_prop

    (** X position of matching point. *)
    method x : float Js.readonly_prop

    (** Y position of matching point. *)
    method y : float Js.readonly_prop
  end

and tooltipBodyLines =
  object
    method before : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

    method lines : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

    method after : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
  end

and tooltipModel =
  object
    (** The items that we are rendering in the tooltip. *)
    method dataPoints : tooltipItem Js.t Js.js_array Js.t Js.readonly_prop

    (** Positioning. *)

    method xPadding : int Js.readonly_prop

    method yPadding : int Js.readonly_prop

    method xAlign : Js.js_string Js.t Js.readonly_prop

    method yAlign : Js.js_string Js.t Js.readonly_prop

    (** X and Y readonly_properties are the top left of the tooltip. *)

    method x : float Js.readonly_prop

    method y : float Js.readonly_prop

    method width : float Js.readonly_prop

    method height : float Js.readonly_prop

    (** Where the tooltip points to. *)
    method caretX : int Js.readonly_prop

    method caretY : int Js.readonly_prop

    (** Body.
      The body lines that need to be rendered.
      Each object contains 3 parameters.
      [before] - lines of text before the line with the color square
      [lines] - lines of text to render as the main item with color square
      [after] - lines of text to render after the main lines. *)

    method body : tooltipBodyLines Js.t Js.readonly_prop

    method beforeBody : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

    method afterBody : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

    method bodyFontColor : Color.t Js.t Js.readonly_prop

    method __bodyFontFamily : Js.js_string Js.t Js.readonly_prop

    method __bodyFontStyle : Js.js_string Js.t Js.readonly_prop

    method __bodyAlign : Js.js_string Js.t Js.readonly_prop

    method bodyFontSize : int Js.readonly_prop

    method bodySpacing : int Js.readonly_prop

    (** Title. Lines of text that form the title. *)

    method title : Js.js_string Js.t Indexable.t Js.readonly_prop

    method titleFontColor : Color.t Js.t Js.readonly_prop

    method __titleFontFamily : Js.js_string Js.t Js.readonly_prop

    method __titleFontStyle : Js.js_string Js.t Js.readonly_prop

    method titleFontSize : int Js.readonly_prop

    method __titleAlign : Js.js_string Js.t Js.readonly_prop

    method titleSpacing : int Js.readonly_prop

    method titleMarginBottom : int Js.readonly_prop

    (** Footer. Lines of text that form the footer. *)

    method footer : Js.js_string Js.t Indexable.t Js.readonly_prop

    method footerFontColor : Color.t Js.t Js.readonly_prop

    method __footerFontFamily : Js.js_string Js.t Js.readonly_prop

    method __footerFontStyle : Js.js_string Js.t Js.readonly_prop

    method footerFontSize : int Js.readonly_prop

    method __footerAlign : Js.js_string Js.t Js.readonly_prop

    method footerSpacing : int Js.readonly_prop

    method footerMarginTop : int Js.readonly_prop

    (** Appearance. *)

    method caretSize : int Js.readonly_prop

    method caretPadding : int Js.readonly_prop

    method cornerRadius : int Js.readonly_prop

    method backgroundColor : Color.t Js.t Js.readonly_prop

    (** Colors to render for each item in body. This is the color of the
      squares in the tooltip. *)
    method labelColors : Color.t Js.t Js.js_array Js.t Js.readonly_prop

    method labelTextColors : Color.t Js.t Js.js_array Js.t Js.readonly_prop

    (** Zero opacity is a hidden tooltip. *)
    method opacity : float Js.readonly_prop

    method legendColorBackground : Color.t Js.t Js.readonly_prop

    method displayColors : bool Js.t Js.readonly_prop

    method borderColor : Color.t Js.t Js.readonly_prop

    method borderWidth : int Js.readonly_prop
  end

and tooltipCallbacks =
  object
    (** Returns the text to render before the title. *)
    method beforeTitle :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns the text to render as the title of the tooltip. *)
    method title :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns the text to render after the title. *)
    method afterTitle :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns the text to render before the body section. *)
    method beforeBody :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns the text to render before an individual label.
      This will be called for each item in the tooltip. *)
    method beforeLabel : (tooltip Js.t, tooltipItem Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns the text to render for an individual item in the tooltip. *)
    method label : (tooltip Js.t, tooltipItem Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns the colors to render for the tooltip item. *)
    method labelColor : (tooltip Js.t, tooltipItem Js.t, chart Js.t) tooltip_cb Js.prop

    (** Returns the colors for the text of the label for the tooltip item. *)
    method labelTextColor :
      (tooltip Js.t, tooltipItem Js.t, chart Js.t) tooltip_cb Js.prop

    (** Returns text to render after an individual label. *)
    method afterLabel : (tooltip Js.t, tooltipItem Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns text to render after the body section. *)
    method afterBody :
      (tooltip Js.t, tooltipItem Js.t Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns text to render before the footer section. *)
    method beforeFooter :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Returns text to render as the footer of the tooltip. *)
    method footer :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop

    (** Text to render after the footer section. *)
    method afterFooter :
      (tooltip Js.t, tooltipItem Js.js_array Js.t, data Js.t) tooltip_cb Js.prop
  end

and tooltip =
  object ('self)
    (** Are on-canvas tooltips enabled. *)
    method enabled : bool Js.t Js.prop

    (** Custom tooltip callback. *)
    method custom : (tooltipModel Js.t -> unit) Js.callback Js.opt Js.prop

    (** Sets which elements appear in the tooltip. *)
    method mode : Interaction_mode.t Js.t Js.prop

    (** If [true], the tooltip mode applies only when the mouse position
      intersects with an element. If [false], the mode will be applied
      at all times. *)
    method intersect : bool Js.t Js.prop

    (** Defines which directions are used in calculating distances.
      Defaults to [x] for index mode and [xy] in [dataset] and [nearest]
      modes. *)
    method axis : Hover_axis.t Js.t Js.prop

    (** The mode for positioning the tooltip. *)
    method position : Tooltip_position.t Js.prop

    (** Callbacks. *)
    method callbacks : tooltipCallbacks Js.t Js.prop

    (** Sort tooltip items. *)
    method itemSort :
      ('self, tooltipItem Js.t -> tooltipItem Js.t -> data Js.t -> int) Js.meth_callback
      Js.optdef_prop

    (** Filter tooltip items. *)
    method filter :
      ('self, tooltipItem Js.t -> data Js.t -> bool Js.t) Js.meth_callback Js.optdef_prop

    (** Background color of the tooltip. *)
    method backgroundColor : Color.t Js.t Js.prop

    (** Title font. *)
    method titleFontFamily : Js.js_string Js.t Js.optdef_prop

    (** Title font size. *)
    method titleFontSize : int Js.optdef_prop

    (** Title font style *)
    method titleFontStyle : Js.js_string Js.t Js.optdef_prop

    (** Title font color *)
    method titleFontColor : Color.t Js.t Js.optdef_prop

    (** Spacing to add to top and bottom of each title line. *)
    method titleSpacing : int Js.prop

    (** Margin to add on bottom of title section. *)
    method titleMarginBottom : int Js.prop

    (** Body line font. *)
    method bodyFontFamily : Js.js_string Js.t Js.optdef_prop

    (** Body font size. *)
    method bodyFontSize : int Js.optdef_prop

    (** Body font style. *)
    method bodyFontStyle : Js.js_string Js.t Js.optdef_prop

    (** Body font color. *)
    method bodyFontColor : Color.t Js.t Js.optdef_prop

    (** Spacing to add to top and bottom of each tooltip item. *)
    method bodySpacing : int Js.prop

    (** Footer font. *)
    method footerFontFamily : Js.js_string Js.t Js.optdef_prop

    (** Footer font size. *)
    method footerFontSize : int Js.optdef_prop

    (** Footer font style. *)
    method footerFontStyle : Js.js_string Js.t Js.optdef_prop

    (** Footer font color. *)
    method footerFontColor : Color.t Js.t Js.optdef_prop

    (** Spacing to add to top and bottom of each footer line. *)
    method footerSpacing : int Js.prop

    (** Margin to add before drawing the footer. *)
    method footerMarginTop : int Js.prop

    (** Padding to add on left and right of tooltip. *)
    method xPadding : int Js.prop

    (** Padding to add on top and bottom of tooltip. *)
    method yPadding : int Js.prop

    (** Extra distance to move the end of the tooltip arrow
      away from the tooltip point. *)
    method caretPadding : int Js.prop

    (** Size, in px, of the tooltip arrow. *)
    method caretSize : int Js.prop

    (** Radius of tooltip corner curves. *)
    method cornerRadius : int Js.prop

    (** Color to draw behind the colored boxes when multiple
      items are in the tooltip. *)
    method multyKeyBackground : Color.t Js.t Js.prop

    (** If [true], color boxes are shown in the tooltip. *)
    method displayColors : bool Js.t Js.prop

    (** Color of the border. *)
    method borderColor : Color.t Js.t Js.prop

    (** Size of the border. *)
    method borderWidth : int Js.prop
  end

(** {2 Interactions} *)

and hover =
  object
    (** Sets which elements appear in the tooltip. *)
    method mode : Interaction_mode.t Js.t Js.prop

    (** If true, the hover mode only applies when the mouse
      position intersects an item on the chart. *)
    method intersect : bool Js.t Js.prop

    (** Defines which directions are used in calculating distances.
      Defaults to [x] for index mode and [xy] in [dataset] and [nearest]
      modes. *)
    method axis : Hover_axis.t Js.t Js.prop

    (** Duration in milliseconds it takes to animate hover style changes. *)
    method animationDuration : int Js.prop
  end

(** {2 Elements} *)

and pointElement =
  object
    (** Point radius. *)
    method radius : int Js.prop

    (** Point style. *)
    method pointStyle : Point_style.t Js.t Js.prop

    (** Point rotation (in degrees). *)
    method rotation : int Js.optdef_prop

    (** Point fill color. *)
    method backgroundColor : Color.t Js.t Js.prop

    (** Point stroke width. *)
    method borderWidth : int Js.prop

    (** Point stroke color. *)
    method borderColor : Color.t Js.t Js.prop

    (** Extra radius added to point radius for hit detection. *)
    method hitRadius : int Js.prop

    (** Point radius when hovered. *)
    method hoverRadius : int Js.prop

    (** Stroke width when hovered. *)
    method hoverBorderWidth : int Js.prop
  end

and lineElement =
  object
    (** Bézier curve tension (0 for no Bézier curves). *)
    method tension : float Js.prop

    (** Line fill color. *)
    method backgroundColor : Color.t Js.t Js.prop

    (** Line stroke width. *)
    method borderWidth : int Js.prop

    (** Line stroke color. *)
    method borderColor : Color.t Js.t Js.prop

    (** Line cap style. *)
    method borderCapStyle : Line_cap.t Js.t Js.prop

    (** Line dash. *)
    method borderDash : line_dash Js.prop

    (** Line dash offset. *)
    method borderDashOffset : line_dash_offset Js.prop

    (** Line join style. *)
    method borderJoinStyle : Line_join.t Js.t Js.prop

    (** [true] to keep Bézier control inside the chart,
      [false] for no restriction.*)
    method capBezierPoints : bool Js.t Js.prop

    (** Fill location. *)
    method fill : Fill.t Js.t Js.prop

    (** [true] to show the line as a stepped line (tension will be ignored). *)
    method stepped : bool Js.t Js.optdef_prop
  end

and rectangleElement =
  object
    (** Bar fill color. *)
    method backgroundColor : Js.js_string Js.prop

    (** Bar stroke width. *)
    method borderWidth : int Js.prop

    (** Bar stroke color. *)
    method borderColor : Color.t Js.t Js.prop

    (** Skipped (excluded) border: 'bottom', 'left', 'top' or 'right'. *)
    method borderSkipped : Position.t Js.t Js.prop
  end

and arcElement =
  object
    (** Arc fill color. *)
    method backgroundColor : Color.t Js.t Js.prop

    (** Arc stroke alignment. *)
    method borderAlign : Js.js_string Js.t Js.prop

    (** Arc stroke color. *)
    method borderColor : Color.t Js.t Js.prop

    (** Arc stroke width. *)
    method borderWidth : int Js.prop
  end

and elements =
  object
    (** Point elements are used to represent the points
      in a line chart or a bubble chart. *)
    method point : pointElement Js.t Js.prop

    (** Line elements are used to represent the line in a line chart. *)
    method line : lineElement Js.t Js.prop

    (** Rectangle elements are used to represent the bars in a bar chart. *)
    method rectangle : rectangleElement Js.t Js.prop

    (** Arcs are used in the polar area, doughnut and pie charts. *)
    method arc : arcElement Js.t Js.prop
  end

(** {2 Options} *)

and chartSize =
  object
    method width : int Js.readonly_prop

    method height : int Js.readonly_prop
  end

(** {2 Chart} *)

(** The configuration is used to change how the chart behaves.
    There are properties to control styling, fonts, the legend, etc. *)
and chartOptions =
  object
    (** Chart.js animates charts out of the box.
      A number of options are provided to configure how the animation
      looks and how long it takes. *)
    method _animation : animation Js.t Js.prop

    (** Layout configurations. *)
    method layout : layout Js.t Js.prop

    (** The chart legend displays data about the datasets
      that are appearing on the chart. *)
    method legend : legend Js.t Js.prop

    (** The chart title defines text to draw at the top of the chart. *)
    method title : title Js.t Js.prop

    method hover : hover Js.t Js.prop

    method tooltips : tooltip Js.t Js.prop

    (** While chart types provide settings to configure the styling
      of each dataset, you sometimes want to style all datasets the same way.
      A common example would be to stroke all of the bars in a bar chart with
      the same colour but change the fill per dataset.
      Options can be configured for four different types of elements: arc, lines,
      points, and rectangles. When set, these options apply to all objects
      of that type unless specifically overridden by the configuration attached
      to a dataset. *)
    method elements : elements Js.t Js.prop

    (** Plugins are the most efficient way to customize or change the default
      behavior of a chart. This option allows to define plugins directly in
      the chart [plugins] config (a.k.a. inline plugins). *)
    method plugins : 'a Js.t Js.prop

    (** Sometimes you need a very complex legend. In these cases, it makes sense
      to generate an HTML legend. Charts provide a generateLegend() method on their
      prototype that returns an HTML string for the legend.
      NOTE [legendCallback] is not called automatically and you must call
      [generateLegend] yourself in code when creating a legend using this method. *)
    method legendCallback : (chart Js.t -> Js.js_string Js.t) Js.callback Js.optdef_prop

    (** Resizes the chart canvas when its container does. *)
    method responsive : bool Js.t Js.prop

    (** Duration in milliseconds it takes to animate
      to new size after a resize event. *)
    method responsiveAnimationDuration : int Js.prop

    (** Maintain the original canvas aspect ratio (width / height) when resizing. *)
    method maintainAspectRatio : bool Js.t Js.prop

    (** Canvas aspect ratio (i.e. width / height, a value of 1
      representing a square canvas). Note that this option is
      ignored if the height is explicitly defined either as
      attribute or via the style. *)
    method aspectRatio : float Js.optdef_prop

    (** Called when a resize occurs. Gets passed two arguments:
      the chart instance and the new size. *)
    method onResize :
      (chart Js.t -> chartSize Js.t -> unit) Js.callback Js.opt Js.optdef_prop

    (** Override the window's default devicePixelRatio. *)
    method devicePixelRatio : float Js.optdef_prop

    (** The events option defines the browser events that
      the chart should listen to for tooltips and hovering. *)
    method events : Js.js_string Js.t Js.js_array Js.t Js.prop

    (** Called when any of the events fire.
      Called in the context of the chart and passed the event
      and an array of active elements (bars, points, etc). *)
    method onHover :
      ( chart Js.t
      , Dom_html.event Js.t -> 'a Js.t Js.js_array Js.t -> unit )
      Js.meth_callback
      Js.opt
      Js.optdef_prop

    (** Called if the event is of type 'mouseup' or 'click'.
          Called in the context of the chart and passed the event
          and an array of active elements. *)
    method onClick :
      ( chart Js.t
      , Dom_html.event Js.t -> 'a Js.t Js.js_array Js.t -> unit )
      Js.meth_callback
      Js.opt
      Js.optdef_prop
  end

and chartConfig =
  object
    method data : data Js.t Js.prop

    method options : chartOptions Js.t Js.prop

    method _type : Js.js_string Js.t Js.prop
  end

and chart =
  object ('self)
    method id : int Js.readonly_prop

    method height : int Js.readonly_prop

    method width : int Js.readonly_prop

    method offsetX : int Js.readonly_prop

    method offsetY : int Js.readonly_prop

    method borderWidth : int Js.readonly_prop

    method animating : bool Js.t Js.readonly_prop

    method aspectRatio : float Js.readonly_prop

    method canvas : Dom_html.canvasElement Js.t Js.readonly_prop

    method ctx : Dom_html.canvasRenderingContext2D Js.t Js.readonly_prop

    method data : data Js.t Js.prop

    method _options : chartOptions Js.t Js.prop

    method _config : chartConfig Js.t Js.prop

    (** {2 Chart API}*)

    (** Use this to destroy any chart instances that are created.
      This will clean up any references stored to the chart object within Chart.js,
      along with any associated event listeners attached by Chart.js.
      This must be called before the canvas is reused for a new chart. *)
    method destroy : unit Js.meth

    method update : unit Js.meth

    (** Triggers an update of the chart.
      This can be safely called after updating the data object.
      This will update all scales, legends, and then re-render the chart.
      A config object can be provided with additional configuration for
      the update process. This is useful when update is manually called inside
      an event handler and some different animation is desired.
      The following properties are supported:
      [duration]: Time for the animation of the redraw in milliseconds
      [lazy]: If [true], the animation can be interrupted by other animations
      [easing]: The animation easing function. *)
    method update_withConfig : #updateConfig Js.t -> unit Js.meth

    (** Reset the chart to it's state before the initial animation.
      A new animation can then be triggered using [update]. *)
    method reset : unit Js.meth

    method render : unit Js.meth

    (** Triggers a redraw of all chart elements.
      Note, this does not update elements for new data.
      Use [update] in that case.
      See [update_withConfig] for more details on the config object. *)
    method render_withConfig : #updateConfig Js.t -> unit Js.meth

    (** Use this to stop any current animation loop.
      This will pause the chart during any current animation frame.
      Call [render] to re-animate. *)
    method stop : 'self Js.t Js.meth

    (** Use this to manually resize the canvas element.
      This is run each time the canvas container is resized,
      but you can call this method manually if you change the size
      of the canvas nodes container element. *)
    method resize : 'self Js.t Js.meth

    (** Will clear the chart canvas. Used extensively internally between
      animation frames, but you might find it useful. *)
    method clear : 'self Js.t Js.meth

    (** This returns a base 64 encoded string of the chart in it's current state. *)
    method toBase64Image : Js.js_string Js.t Js.meth

    (** Returns an HTML string of a legend for that chart.
      The legend is generated from the legendCallback in the options. *)
    method generateLegend : Js.js_string Js.t Js.meth

    (** Looks for the dataset that matches the current index and returns that metadata.
      This returned data has all of the metadata that is used to construct the chart.
      The [data] property of the metadata will contain information about each point,
      rectangle, etc. depending on the chart type. *)
    method getDatasetMeta : int -> 'a Js.t Js.meth
  end

val empty_animation : unit -> animation Js.t

val empty_layout : unit -> layout Js.t

val empty_legend_labels : unit -> legendLabels Js.t

val empty_legend : unit -> legend Js.t

val empty_title : unit -> title Js.t

val empty_tooltip_model : unit -> tooltipModel Js.t

val empty_tooltip_callbacks : unit -> tooltipCallbacks Js.t

val empty_tooltip : unit -> tooltip Js.t

val empty_hover : unit -> hover Js.t

val empty_point_element : unit -> pointElement Js.t

val empty_line_element : unit -> lineElement Js.t

val empty_rectangle_element : unit -> rectangleElement Js.t

val empty_arc_element : unit -> arcElement Js.t

val empty_elements : unit -> elements Js.t

val empty_update_config : unit -> updateConfig Js.t

(** {1 Charts} *)

(** {2 Line Chart} *)

class type ['a] lineOptionContext =
  object
    method chart : lineChart Js.t Js.readonly_prop

    method dataIndex : int Js.readonly_prop

    method dataset : 'a lineDataset Js.t Js.readonly_prop

    method datasetIndex : int Js.readonly_prop
  end

and lineScales =
  object
    method xAxes : #cartesianAxis Js.t Js.js_array Js.t Js.prop

    method yAxes : #cartesianAxis Js.t Js.js_array Js.t Js.prop
  end

and lineOptions =
  object
    inherit chartOptions

    method animation : animation Js.t Js.prop

    method scales : lineScales Js.t Js.prop

    (** If [false], the lines between points are not drawn. *)
    method showLines : bool Js.t Js.prop

    (** If [false], NaN data causes a break in the line. *)
    method spanGaps : bool Js.t Js.prop
  end

and lineConfig =
  object
    method data : data Js.t Js.prop

    method options : lineOptions Js.t Js.prop

    method _type : Js.js_string Js.t Js.prop
  end

and ['a] lineDataset =
  object
    inherit dataset

    method data : 'a Js.js_array Js.t Js.prop

    (** {2 General} *)

    (** The ID of the x axis to plot this dataset on. *)
    method xAxisID : Js.js_string Js.t Js.optdef_prop

    (** The ID of the y axis to plot this dataset on. *)
    method yAxisID : Js.js_string Js.t Js.optdef_prop

    (** {2 Point styling} *)

    (** The fill color for points. *)
    method pointBackgroundColor :
      ('a lineOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t
      Js.optdef_prop

    (** The border color for points. *)
    method pointBorderColor :
      ('a lineOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t
      Js.optdef_prop

    (** The width of the point border in pixels. *)
    method pointBorderWidth :
      ('a lineOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The pixel size of the non-displayed point that reacts to mouse events. *)
    method pointHitRadius :
      ('a lineOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The radius of the point shape. If set to 0, the point is not rendered. *)
    method pointRadius :
      ('a lineOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The rotation of the point in degrees. *)
    method pointRotation :
      ('a lineOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** Style of the point. *)
    method pointStyle : Point_style.t Js.t Js.optdef_prop

    (** {2 Line styling} *)

    (** The line fill color. *)
    method backgroundColor : Color.t Js.t Js.optdef_prop

    (** Cap style of the line. *)
    method borderCapStyle : Line_cap.t Js.t Js.optdef_prop

    (** The line color. *)
    method borderColor : Color.t Js.t Js.optdef_prop

    (** Length and spacing of dashes. *)
    method borderDash : line_dash Js.optdef_prop

    (** Offset for line dashes. *)
    method borderDashOffset : line_dash_offset Js.optdef_prop

    (** Line joint style. *)
    method borderJoinStyle : Line_join.t Js.t Js.optdef_prop

    (** The line width (in pixels). *)
    method borderWidth : int Js.optdef_prop

    (** How to fill the area under the line. *)
    method fill : Line_fill.t Js.t Js.optdef_prop

    (** Bezier curve tension of the line. Set to 0 to draw straightlines.
      This option is ignored if monotone cubic interpolation is used. *)
    method lineTension : float Js.optdef_prop

    (** If [false], the line is not drawn for this dataset. *)
    method showLine : bool Js.t Js.optdef_prop

    (** If [true], lines will be drawn between points with no or null data.
      If [false], points with NaN data will create a break in the line. *)
    method spanGaps : bool Js.t Js.optdef_prop

    (** {2 Interactions} *)

    (** Point background color when hovered. *)
    method pointHoverBackgroundColor :
      ('a lineOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t
      Js.optdef_prop

    (** Point border color when hovered. *)
    method pointHoverBorderColor :
      ('a lineOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t
      Js.optdef_prop

    (** Border width of point when hovered. *)
    method pointHoverBorderWidth :
      ('a lineOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The radius of the point when hovered. *)
    method pointHoverRadius :
      ('a lineOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** {2 Cubic Interpolation Mode} *)

    (** The [default] and [monotone] interpolation modes are supported.
      The [default] algorithm uses a custom weighted cubic interpolation,
      which produces pleasant curves for all types of datasets.
      The [monotone] algorithm is more suited to [y = f(x)] datasets : it preserves
      monotonicity (or piecewise monotonicity) of the dataset being interpolated,
      and ensures local extremums (if any) stay at input data points.
      If left untouched (undefined), the global
      [options.elements.line.cubicInterpolationMode] property is used. *)
    method cubicInterpolationMode : Interpolation_mode.t Js.t Js.optdef_prop

    (** {2 Stepped Line} *)

    (** The following values are supported for steppedLine.
      [false]: No Step Interpolation (default)
      [true]: Step-before Interpolation (eq. 'before')
      ['before']: Step-before Interpolation
      ['after']: Step-after Interpolation
      ['middle']: Step-middle Interpolation
      If the [steppedLine] value is set to anything other than [false],
      [lineTension] will be ignored.*)
    method steppedLine : Stepped_line.t Js.t Js.optdef_prop
  end

and lineChart =
  object
    inherit chart

    method options : lineOptions Js.t Js.prop

    method config : lineConfig Js.t Js.prop
  end

val empty_line_scales : unit -> lineScales Js.t

val empty_line_options : unit -> lineOptions Js.t

val empty_line_dataset : unit -> 'a lineDataset Js.t

(** {2 Bar Chart} *)

class type barAxis =
  object
    (** Percent (0-1) of the available width each bar should be within
      the category width. 1.0 will take the whole category width and
      put the bars right next to each other. *)
    method barPercentage : float Js.prop

    (** Percent (0-1) of the available width each category should be within
      the sample width. *)
    method categoryPercentage : float Js.prop

    (** Manually set width of each bar in pixels. If set to 'flex',
      it computes "optimal" sample widths that globally arrange bars side by side.
      If not set (default), bars are equally sized based on the smallest interval. *)
    method barThickness : Bar_thickness.t Js.t Js.optdef_prop

    (** Set this to ensure that bars are not sized thicker than this. *)
    method maxBarThickness : float Js.optdef_prop

    (** Set this to ensure that bars have a minimum length in pixels. *)
    method minBarLength : float Js.optdef_prop

    method stacked : bool Js.t Js.optdef_prop
  end

class type cateroryBarAxis =
  object
    inherit categoryAxis

    inherit barAxis
  end

class type linearBarAxis =
  object
    inherit linearAxis

    inherit barAxis
  end

class type logarithmicBarAxis =
  object
    inherit logarithmicAxis

    inherit barAxis
  end

class type timeBarAxis =
  object
    inherit timeAxis

    inherit barAxis
  end

class type barScales =
  object
    method xAxes : #barAxis Js.t Js.js_array Js.t Js.prop

    method yAxes : #barAxis Js.t Js.js_array Js.t Js.prop
  end

class type ['a] barOptionContext =
  object
    method chart : barChart Js.t Js.readonly_prop

    method dataIndex : int Js.readonly_prop

    method dataset : 'a barDataset Js.t Js.readonly_prop

    method datasetIndex : int Js.readonly_prop
  end

and barOptions =
  object
    inherit chartOptions

    method animation : animation Js.t Js.prop

    method scales : barScales Js.t Js.prop
  end

and barConfig =
  object
    method data : data Js.t Js.prop

    method options : barOptions Js.t Js.prop

    method _type : Js.js_string Js.t Js.prop
  end

and ['a] barDataset =
  object
    inherit dataset

    method data : 'a Js.js_array Js.t Js.prop

    (** {2 General} *)

    (** The ID of the x axis to plot this dataset on. *)
    method xAxisID : Js.js_string Js.t Js.optdef_prop

    (** The ID of the y axis to plot this dataset on. *)
    method yAxisID : Js.js_string Js.t Js.optdef_prop

    (** {2 Styling} *)

    (** The bar background color. *)
    method backgroundColor :
      ('a barOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The bar border color. *)
    method borderColor :
      ('a barOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The edge to skip when drawing bar.
      This setting is used to avoid drawing the bar stroke at the base of the fill.
      In general, this does not need to be changed except when creating chart types
      that derive from a bar chart.
      Note: for negative bars in vertical chart, top and bottom are flipped.
      Same goes for left and right in horizontal chart.
      Options are:
      ['bottom'],
      ['left'],
      ['top'],
      ['right'],
      [false] *)
    method borderSkipped :
      ('a barOptionContext Js.t, Position.t Js.t Or_false.t Js.t) Scriptable_indexable.t
      Js.t
      Js.optdef_prop

    (** The bar border width (in pixels).
      If this value is a number, it is applied to all sides of the rectangle
      (left, top, right, bottom), except [borderSkipped]. If this value is
      an object, the [left] property defines the left border width. Similarly
      the [right], [top] and [bottom] properties can also be specified.
      Omitted borders and [borderSkipped] are skipped. *)
    method borderWidth :
      ('a barOptionContext Js.t, Padding.t Js.t) Scriptable_indexable.t Js.t
      Js.optdef_prop

    (** {2 Interactions}
      All these values, if undefined, fallback to the associated
      [elements.rectangle.*] options. *)

    (** The bar background color when hovered. *)
    method hoverBackgroundColor : Color.t Js.t Indexable.t Js.t Js.optdef_prop

    (** The bar border color when hovered. *)
    method hoverBorderColor : Color.t Js.t Indexable.t Js.t Js.optdef_prop

    (** The bar border width when hovered (in pixels). *)
    method hoverBorderWidth : Color.t Js.t Indexable.t Js.t Js.optdef_prop

    (** The ID of the group to which this dataset belongs to
      (when stacked, each group will be a separate stack). *)
    method stack : Js.js_string Js.t Js.optdef_prop
  end

and barChart =
  object
    inherit chart

    method options : barOptions Js.t Js.prop

    method config : barConfig Js.t Js.prop
  end

val empty_bar_axis : unit -> cateroryBarAxis Js.t

val empty_linear_bar_axis : unit -> linearBarAxis Js.t

val empty_logarithmic_bar_axis : unit -> logarithmicBarAxis Js.t

val empty_time_bar_axis : unit -> timeBarAxis Js.t

val empty_bar_scales : unit -> barScales Js.t

val empty_bar_options : unit -> barOptions Js.t

val empty_bar_dataset : unit -> 'a barDataset Js.t

(** {2 Pie Chart} *)

class type ['a] pieOptionContext =
  object
    method chart : pieChart Js.t Js.readonly_prop

    method dataIndex : int Js.readonly_prop

    method dataset : 'a pieDataset Js.t Js.readonly_prop

    method datasetIndex : int Js.readonly_prop
  end

and pieAnimation =
  object
    inherit animation

    (** If [true], the chart will animate in with a rotation animation. *)
    method animateRotate : bool Js.t Js.prop

    (** If true, will animate scaling the chart from the center outwards. *)
    method animateScale : bool Js.t Js.prop
  end

and pieOptions =
  object
    inherit chartOptions

    method animation : pieAnimation Js.t Js.prop

    (** The percentage of the chart that is cut out of the middle. *)
    method cutoutPercentage : float Js.prop

    (** Starting angle to draw arcs from. *)
    method rotation : float Js.prop

    (** Sweep to allow arcs to cover. *)
    method circumference : float Js.prop
  end

and pieConfig =
  object
    method data : data Js.t Js.prop

    method options : pieOptions Js.t Js.prop

    method _type : Js.js_string Js.t Js.prop
  end

and ['a] pieDataset =
  object
    inherit dataset

    method data : 'a Js.js_array Js.t Js.prop

    (** {2 Styling} *)

    (** Arc background color. *)
    method backgroundColor :
      ('a pieOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t Js.optdef_prop

    (** Arc border color. *)
    method borderColor :
      ('a pieOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t Js.optdef_prop

    (** Arc border width (in pixels). *)
    method borderWidth :
      ('a pieOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** The relative thickness of the dataset.
      Providing a value for weight will cause the pie or doughnut dataset
      to be drawn with a thickness relative to the sum of all the dataset
      weight values. *)
    method weight : float Js.optdef_prop

    (** {2 Interactions} *)

    (** Arc background color when hovered. *)
    method hoverBackgroundColor :
      ('a pieOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t Js.optdef_prop

    (** Arc border color when hovered. *)
    method hoverBorderColor :
      ('a pieOptionContext Js.t, Color.t Js.t) Scriptable_indexable.t Js.t Js.optdef_prop

    (** Arc border width when hovered (in pixels). *)
    method hoverBorderWidth :
      ('a pieOptionContext Js.t, int) Scriptable_indexable.t Js.t Js.optdef_prop

    (** {2 Border Alignment} *)

    (** The following values are supported for [borderAlign]:
      ['center'] (default),
      ['inner'].
      When ['center'] is set, the borders of arcs next to each other will overlap.
      When ['inner'] is set, it is guaranteed that all the borders are not overlap. *)
    method borderAlign : Pie_border_align.t Js.t Js.optdef_prop
  end

and pieChart =
  object
    inherit chart

    method options : pieOptions Js.t Js.prop

    method config : pieConfig Js.t Js.prop
  end

val empty_pie_animation : unit -> pieAnimation Js.t

val empty_pie_options : unit -> pieOptions Js.t

val empty_pie_dataset : unit -> 'a pieDataset Js.t

module Axis : sig
  type 'a typ

  val cartesian_category : categoryAxis typ

  val cartesian_linear : linearAxis typ

  val cartesian_logarithmic : logarithmicAxis typ

  val cartesian_time : timeAxis typ

  val of_string : string -> 'a typ
end

module Chart : sig
  type 'a typ

  val line : lineChart typ

  val bar : barChart typ

  val horizontal_bar : barChart typ

  val pie : pieChart typ

  val doughnut : pieChart typ

  val of_string : string -> 'a typ
end

(** {1 Type Coercion} *)

module CoerceTo : sig
  val line : #chart Js.t -> lineChart Js.t Js.opt

  val bar : #chart Js.t -> barChart Js.t Js.opt

  val horizontalBar : #chart Js.t -> barChart Js.t Js.opt

  val pie : #chart Js.t -> pieChart Js.t Js.opt

  val doughnut : #chart Js.t -> pieChart Js.t Js.opt

  val cartesianCategory : #axis Js.t -> categoryAxis Js.t Js.opt

  val cartesianLinear : #axis Js.t -> linearAxis Js.t Js.opt

  val cartesianLogarithmic : #axis Js.t -> logarithmicAxis Js.t Js.opt

  val cartesianTime : #axis Js.t -> timeAxis Js.t Js.opt
end

(** {1 Creating an Axis} *)

val create_axis : 'a Axis.typ -> 'a Js.t

(** {1 Creating a Chart} *)

val chart_from_canvas :
     'a Chart.typ
  -> data Js.t
  -> #chartOptions Js.t
  -> Dom_html.canvasElement Js.t
  -> 'a Js.t

val chart_from_ctx :
     'a Chart.typ
  -> data Js.t
  -> #chartOptions Js.t
  -> Dom_html.canvasRenderingContext2D Js.t
  -> 'a Js.t

val chart_from_id : 'a Chart.typ -> data Js.t -> #chartOptions Js.t -> string -> 'a Js.t
