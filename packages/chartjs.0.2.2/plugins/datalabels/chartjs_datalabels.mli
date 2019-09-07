open Js_of_ocaml
open Chartjs

module Align : sig
  type t

  val center : t Js.t
  (** The label is centered on the anchor point. *)

  val start : t Js.t
  (** The label is positioned before the anchor point, following the same direction. *)

  val _end : t Js.t
  (** The label is positioned after the anchor point, following the same direction. *)

  val right : t Js.t
  (** The label is positioned to the right of the anchor point (0°). *)

  val bottom : t Js.t
  (** The label is positioned to the bottom of the anchor point (90°). *)

  val left : t Js.t
  (** The label is positioned to the left of the anchor point (180°). *)

  val top : t Js.t
  (** The label is positioned to the top of the anchor point (270°). *)

  val degree : float -> t Js.t
  (** A number representing the clockwise angle (in degree). *)
end

module Anchor : sig
  type t

  val center : t Js.t
  (** Element center. *)

  val start : t Js.t
  (** Lowest element boundary. *)

  val _end : t Js.t
  (** Highest element boundary. *)
end

module Visibility : sig
  type t

  val of_bool : bool -> t Js.t
  (** - [true] (default) - the label is drawn
      - [false] -  the label is hidden *)

  val auto : t Js.t
  (** The label is hidden if it overlap with another label *)
end

module Text_align : sig
  type t

  val start : t Js.t
  (** The text is left-aligned. *)

  val center : t Js.t
  (** The text is centered. *)

  val _end : t Js.t
  (** The text is right-aligned. *)

  val left : t Js.t
  (** Alias for ['start']. *)

  val right : t Js.t
  (** Alias for ['end']. *)
end

class type font =
  object
    method family : Js.js_string Js.t Js.optdef_prop

    method size : int Js.optdef_prop

    method style : Js.js_string Js.t Js.optdef_prop

    method weight : Js.js_string Js.t Js.optdef_prop

    method lineHeight : Line_height.t Js.t Js.optdef_prop
  end

(** {1 Options} *)

class type optionContext =
  object
    method active : bool Js.t Js.readonly_prop

    method chart : chart Js.t Js.readonly_prop

    method dataIndex : int Js.readonly_prop

    method dataset : dataset Js.t Js.readonly_prop

    method datasetIndex : int Js.readonly_prop
  end

class type listeners =
  object
    (** The mouse is moved over a label. *)
    method enter : (optionContext Js.t -> bool Js.t) Js.callback Js.optdef_prop

    (** The mouse is moved out of a label. *)
    method leave : (optionContext Js.t -> bool Js.t) Js.callback Js.optdef_prop

    (** The mouse's primary button is pressed and released on a label. *)
    method click : (optionContext Js.t -> bool Js.t) Js.callback Js.optdef_prop
  end

type 'a prop = (optionContext Js.t, 'a) Scriptable_indexable.t Js.t

type 'a formatter = ('a -> optionContext Js.t -> Js.js_string Js.t) Js.callback

class type datalabels =
  object
    method align : Align.t Js.t prop Js.optdef_prop

    method anchor : Anchor.t Js.t prop Js.optdef_prop

    method backgroundColor : Color.t Js.t Js.opt prop Js.optdef_prop

    method borderColor : Color.t Js.t Js.opt prop Js.optdef_prop

    method borderRadius : int prop Js.optdef_prop

    method borderWidth : int prop Js.optdef_prop

    method clamp : bool Js.t prop Js.optdef_prop

    method clip : bool Js.t prop Js.optdef_prop

    method color : Color.t Js.t prop Js.optdef_prop

    method display : Visibility.t Js.t prop Js.optdef_prop

    method font : font Js.t prop Js.optdef_prop

    method formatter : 'a formatter Js.opt Js.optdef_prop

    method listeners : listeners Js.t Js.optdef_prop

    method offset : float prop Js.optdef_prop

    method opacity : float prop Js.optdef_prop

    method padding : Padding.t Js.t prop Js.optdef_prop

    method rotation : float prop Js.optdef_prop

    method textAlign : Text_align.t Js.t prop Js.optdef_prop

    method textStrokeColor : Color.t Js.t prop Js.optdef_prop

    method textStrokeWidth : int prop Js.optdef_prop

    method textShadowBlur : float prop Js.optdef_prop

    method textShadowColor : Color.t Js.t prop Js.optdef_prop
  end

val empty_font : unit -> font Js.t

val empty_listeners : unit -> listeners Js.t

val empty_datalabels_config : unit -> datalabels Js.t

(** {1 Positioning}

    {2 Anchoring}

    An anchor point is defined by an orientation vector and a position
    on the data element. The orientation depends on the scale type
    (vertical, horizontal or radial). The position is calculated based
    on the {!datalabels.anchor} option and the orientation vector.

    Possible anchor values are defined in the {!Anchor} module.

    {%html: <img
      src="https://chartjs-plugin-datalabels.netlify.com/assets/img/anchor.af396841.png"
      alt="chartjs-datalabels"></img> %}

    {2 Clamping}
    The {!datalabels.clamp} option, when [true], enforces the anchor position to be calculated
    based on the visible geometry of the associated element
    (i.e. part inside the chart area).

    {b NOTE:} If the element is fully hidden (i.e. entirely outside the chart area),
    anchor points will not be adjusted and thus will also be outside
    the viewport.

    {%html: <img
      src="https://chartjs-plugin-datalabels.netlify.com/assets/img/clamp.3d93ea42.png"
      alt="chartjs-datalabels"></img> %}

    {2 Alignment and Offset}

    The {!datalabels.align} option defines the position of the
    label relative to the anchor point position and orientation.
    Its value can be expressed either by a number representing
    the clockwise angle (in degree) or by one of the predefined
    string values.

    Possible values for {!datalabels.align} option are defined
    in the {!Align} module.

    The {!datalabels.offset} option represents the distance (in pixels)
    to pull the label away from the anchor point.
    This option is not applicable when align is {!Align.center}.
    Also note that if {!datalabels.align} is {!Align.start}, the label is
    moved in the opposite direction. The default value is [4].

    {%html: <img
      src="https://chartjs-plugin-datalabels.netlify.com/assets/img/align.fe79da09.png"
      alt="chartjs-datalabels"></img> %}

    {2 Rotation}

    The {!datalabels.rotation} option controls the clockwise rotation angle
    (in degrees) of the label, the rotation center point being the label center.
    The default value is [0] (no rotation).

    {2 Visibility}

    The {!datalabels.display} option controls the visibility of labels.
    The option is scriptable, so it's possible to show/hide specific labels.

    Possible values are defined in the {!Visibility} module.

    {2 Overlap}

    The {!Visibility.auto} option can be used to prevent overlapping labels,
    based on the following rules when two labels overlap:

    - if both labels have {!datalabels.display} option set to [true],
    they will be drawn overlapping
    - if both labels have {!datalabels.display} option set to {!Visibility.auto},
    the one with the highest data index will be hidden.
    If labels are at the same data index, the one with the lowest dataset index
    will be hidden
    - if one label has {!datalabels.display} option set to [true] and the other
    one has {!Visibility.auto}, the one with {!Visibility.auto} will be hidden
    (whatever the data/dataset indices)

    {2 Clipping}

    When the {!datalabels.clip} option is [true], the part of the label which
    is outside the chart area will be masked.

    For more information, see
    {{: https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/clip}
    MDN}

    {1 Formatting}

    {2 Text Alignment}

    The {!datalabels.textAlign} option only applies to multiline labels
    and specifies the text alignment being used when drawing the label text.

    {b NOTE:} right-to-left text detection based on the current locale
    is not currently implemented.

    Possible values are defined in the {!Text_align} module.

    For more information, see
    {{: https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/textAlign}
    MDN}

    {1 Events}

    {2 Listeners}

    The {!datalabels.listeners} option allows to register callbacks to be
    notified when an event is detected on a specific label.
    This option is an object where the property is the type of the event
    to listen and the value is a callback with a unique [context] argument.

    The context contains the same information as for scriptable options,
    can be modified (e.g. add new properties) and thus, {b if the callback
    explicitly returns [true]}, the label is updated with the new context
    and the chart re-rendered. This allows to implement visual interactions
    with labels such as highlight, selection, etc.

    Listeners can be registered for any label
    ([options.plugin.datalabels.listener.*]) or for labels of a specific
    dataset ([dataset.datalabels.listeners.*]).

    {e Tip:}
    If no listener is registered, incoming events are immediately ignored,
    preventing extra computation such as intersecting label bounding box.
    That means there should be no performance penalty for configurations
    that don't use events.
*)

(** {1 Configuration} *)

val of_dataset : #dataset Js.t -> datalabels Js.t Js.optdef

val of_chart_options : #chartOptions Js.t -> datalabels Js.t Js.optdef

val of_global : unit -> datalabels Js.t Js.optdef

val set_to_dataset : #dataset Js.t -> datalabels Js.t -> unit

val set_to_chart_options : #chartOptions Js.t -> datalabels Js.t -> unit

val set_globally : datalabels Js.t -> unit

(** The plugin options can be changed at 3 different levels and are evaluated
    with the following priority:
    - per dataset
    - per chart
    - or globally.

    To configure the plugin for a dataset, use {!set_per_dataset}, {!of_dataset}.

    To configure the plugin for a chart, use {!set_per_chart}, {!of_chart}.

    To configure the plugin globally, use {!set_globally}, {!of_global}.
*)
