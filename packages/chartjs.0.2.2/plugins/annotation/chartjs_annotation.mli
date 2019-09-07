open Js_of_ocaml
open Chartjs

module Line_mode : sig
  type t

  val vertical : t Js.t

  val horizontal : t Js.t
end

module Draw_time : sig
  type t

  val afterDraw : t Js.t

  val afterDatasetsDraw : t Js.t

  val beforeDatasetsDraw : t Js.t
end

module Position : sig
  include module type of Position

  val center : t Js.t
end

class type baseAnnotation =
  object
    (** Line/Box. *)
    method _type : Js.js_string Js.t Js.prop

    (** Optional drawTime to control layering,
      overrides global [drawTime] setting. *)
    method drawTime : Draw_time.t Js.t Js.optdef Js.prop

    (** Optional annotation ID (must be unique). *)
    method id : Js.js_string Js.t Js.prop
  end

class type label =
  object
    (** Background color of label. *)
    method backgroundColor : Color.t Js.t Js.prop

    (** Font family of text. *)
    method fontFamily : Js.js_string Js.t Js.prop

    (** Font size of text. *)
    method fontSize : int Js.prop

    (** Font style of text. *)
    method fontStyle : Js.js_string Js.t Js.prop

    (** Font color of text. *)
    method fontColor : Js.js_string Js.t Js.prop

    (** Padding of label to add left/right. *)
    method xPadding : int Js.prop

    (** Padding of label to add top/bottom. *)
    method yPadding : int Js.prop

    (** Radius of label rectangle. *)
    method cornerRadius : int Js.prop

    (** Anchor position of label on line. *)
    method position : Position.t Js.t Js.prop

    (** Adjustment along y-axis (top-bottom) of label relative to above
      number (can be negative).
		  For vertical lines positioned top or bottom, negative values move
		  the label toward the edge, and positive values toward the center. *)
    method xAdjust : int Js.prop

    (** Adjustment along y-axis (top-bottom) of label relative to above
      number (can be negative).
		  For vertical lines positioned top or bottom, negative values move
		  the label toward the edge, and positive values toward the center. *)
    method yAdjust : int Js.prop

    (** Whether the label is enabled and should be displayed. *)
    method enabled : bool Js.t Js.prop

    (** Text to display in label - default is [null].
      Provide an array to display values on a new line. *)
    method content : Js.js_string Js.t Indexable.t Js.t Js.opt Js.prop
  end

class type ['a] lineAnnotation =
  object
    inherit baseAnnotation

    (** Vertical/horizontal line. *)
    method mode : Line_mode.t Js.t Js.prop

    (** ID of the scale to bind onto. *)
    method scaleID : Js.js_string Js.t Js.prop

    (** Data value to draw the line at. *)
    method value : 'a Js.prop

    (** Optional value at which the line draw should end. *)
    method endValue : 'a Js.prop

    (** Line color. *)
    method borderColor : Color.t Js.t Js.prop

    (** Line width. *)
    method borderWidth : int Js.prop

    (** Line dash.
	    @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash> *)
    method borderDash : line_dash Js.prop

    (** Line Dash Offset
	    @see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset> *)
    method borderDashOffset : line_dash_offset Js.prop

    method label : label Js.t Js.prop
  end

class type ['a, 'b] boxAnnotation =
  object
    inherit baseAnnotation

    (** ID of the X scale to bind onto. *)
    method xScaleID : Js.js_string Js.t Js.prop

    (** ID of the Y scale to bind onto. *)
    method yScaleID : Js.js_string Js.t Js.prop

    (** Left edge of the box. In units along the x axis. *)
    method xMin : 'a Js.optdef Js.prop

    (** Right edge of the box. *)
    method xMax : 'a Js.optdef Js.prop

    method yMin : 'b Js.optdef Js.prop

    method yMax : 'b Js.optdef Js.prop

    (** Stroke color. *)
    method borderColor : Color.t Js.t Js.prop

    (** Stroke width. *)
    method borderWidth : int Js.prop

    (** Fill color. *)
    method backgroundColor : Color.t Js.t Js.prop
  end

class type annotation =
  object
    (** Defines when the annotations are drawn.
			This allows positioning of the annotation relative to the other
			elements of the graph.
			@see <http://www.chartjs.org/docs/#advanced-usage-creating-plugins> *)
    method drawTime : Draw_time.t Js.t Js.prop

    (** Mouse events to enable on each annotation.
			Should be an array of one or more browser-supported mouse events
			@see <https://developer.mozilla.org/en-US/docs/Web/Events> *)
    method events : Js.js_string Js.t Js.js_array Js.t Js.prop

    (** Double-click speed in ms used to distinguish single-clicks from
			double-clicks whenever you need to capture both. When listening for
			both click and dblclick, click events will be delayed by this
			amount. *)
    method dblClickSpeed : float Js.prop

    (** Array of annotation configuration objects. *)
    method annotations : #baseAnnotation Js.t Js.js_array Js.t Js.prop
  end

module CoerceTo : sig
  val lineAnnotation : #baseAnnotation Js.t -> 'a lineAnnotation Js.t Js.opt

  val boxAnnotation : #baseAnnotation Js.t -> ('a, 'b) boxAnnotation Js.t Js.opt
end

val coerce_annotation : #baseAnnotation Js.t -> baseAnnotation Js.t

val empty_label : unit -> label Js.t

val empty_box_annotation : unit -> ('a, 'b) boxAnnotation Js.t

val empty_line_annotation : unit -> 'a lineAnnotation Js.t

val empty_annotation_config : unit -> annotation Js.t

val of_chart_options : #chartOptions Js.t -> annotation Js.t Js.optdef

val set_to_chart_options : #chartOptions Js.t -> annotation Js.t -> unit
