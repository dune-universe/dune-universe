(** Module for connecting the waveform tracer to a hardcaml simulator. *)

(** Wraps a hardcaml simulator with a waveform tracer. This returns a simulator
    object with waveform traced. Both simulators are interchangable (ie: calling
    [Sim.cycle] on one cycles the other as well, but only the simulator
    returned from the function traces waveforms.
*)
val wrap
  :  ?cfg:(string * Wave_format.t) list
  -> ('i, 'o) Hardcaml.Cyclesim.t
  -> ('i, 'o) Hardcaml.Cyclesim.t * Wave.t array
