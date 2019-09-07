open Js_of_ocaml

module Axis : sig
  val realtime : Chartjs.timeAxis Chartjs.Axis.typ
end

class type updateConfig =
  object
    inherit Chartjs.updateConfig

    (** A problem with calling the update function for stream data feeds
      is that it can disrupt smooth transition because an update call
      interrupts the current animation and initiates a new one.
      To avoid this, this plugin added the preservation config property
      for the update function.
      If it is set to [true], the current animation won't be interrupted
      and new data can be added without initiating a new animation. *)
    method preservation : bool Js.t Js.optdef_prop
  end

class type streaming =
  object
    (** Duration of the chart in milliseconds
      (how much time of data it will show). *)
    method duration : int Js.optdef_prop

    (** Duration of the data to be kept in milliseconds.
      If not set, old data will be automatically deleted
      as it disappears off the chart. *)
    method ttl : int Js.optdef Js.prop

    (** Delay added to the chart in milliseconds so that upcoming values
      are known before lines are plotted. This makes the chart look
      like a continual stream rather than very jumpy on the right hand side.
      Specify the maximum expected delay. *)
    method delay : int Js.optdef_prop

    (** Refresh interval of data in milliseconds.
      [onRefresh] callback function will be called at this interval. *)
    method refresh : int Js.optdef_prop

    (** Callback function that will be called at a regular interval.
      The callback takes one argument, a reference to the chart object.
      You can update your datasets here. The chart will be automatically
      updated after returning. *)
    method onRefresh : (Chartjs.chart Js.t -> unit) Js.callback Js.opt Js.optdef_prop

    (** Frequency at which the chart is drawn on a display (frames per second).
      This option can be set at chart level but not at axis level.
      Decrease this value to save CPU power. *)
    method frameRate : float Js.optdef_prop

    (** If set to [true], scrolling stops.
      Note that onRefresh callback is called even when this is set to [true]. *)
    method pause : bool Js.t Js.optdef_prop
  end

val empty_update_config : ?preservation:bool -> unit -> updateConfig Js.t

val empty_streaming_config : unit -> streaming Js.t

val of_axis : #Chartjs.axis Js.t -> streaming Js.t Js.optdef

val of_chart_options : #Chartjs.chartOptions Js.t -> streaming Js.t Js.optdef

val of_global : unit -> streaming Js.t Js.optdef

val set_to_axis : #Chartjs.axis Js.t -> streaming Js.t -> unit

val set_to_chart_options : #Chartjs.chartOptions Js.t -> streaming Js.t -> unit

val set_globally : streaming Js.t -> unit
