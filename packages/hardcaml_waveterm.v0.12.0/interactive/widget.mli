open! Import

val run_waves
  :  ?signals_width:int
  -> ?values_width:int
  -> Waves.t
  -> unit Async.Deferred.t

(** Run the waveform viewer and return.  Calls into Async/LWT so MUST NOT BE CALLED from
    within an Async or LWT deferred. *)
val run : ?signals_width:int -> ?values_width:int -> Waves.t -> unit

(** Run the waveform viewer and close the program on exit. *)
val run_and_close : ?signals_width:int -> ?values_width:int -> Waves.t -> unit

(** Run interactive waveterm viewer. [ESC] to quit. *)
val run_interactive_viewer
  :  ?signals_width:int
  -> ?values_width:int
  -> ?display_rules:Display_rules.t
  -> Waveform.t
  -> unit
