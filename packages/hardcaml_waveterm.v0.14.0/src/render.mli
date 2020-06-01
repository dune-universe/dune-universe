open! Import

module Styles : sig
  type t =
    { border : Draw.Style.t option
    ; signals : Draw.Style.t
    ; values : Draw.Style.t
    ; waves : Draw.Style.t
    ; status : Draw.Style.t
    }

  val default : Draw.Style.t -> t
  val black_on_white : t
  val white_on_black : t
  val colour : t -> t
  val colour_on_white : t
  val colour_on_black : t
end

module Bounds : sig
  type t =
    { signals : Draw.rect
    ; values : Draw.rect
    ; waves : Draw.rect
    ; status : Draw.rect
    }

  val expand_for_border : Draw.rect -> Draw.rect
  val shrink_for_border : Draw.rect -> Draw.rect

  val fit_to_window
    :  ?signals:bool
    -> ?values:bool
    -> ?waves:bool
    -> ?status:bool
    -> ?border:bool
    -> Draw.rect
    -> t
end

(** Functions for drawing waves, signal names and values *)

module Make (G : Draw.S) : sig
  (** get width code and actual width in chars *)
  val get_wave_width : int * Wave.t -> int * int

  (** get height code and actual height in chars *)
  val get_wave_height : int * Wave.t -> int * int

  (** max width of name window *)
  val get_max_signal_width : Waves.t -> int

  (** max width of values window.  Needs to evaluate the entire waveform. *)
  val get_max_value_width : Waves.t -> int

  (** gets an estimate fo the max with of values. Inaccruate for the constructors [U], [S]
      and [F]. *)
  val get_estimated_max_value_width : Waves.t -> int

  (** max no of wave cycles *)
  val get_max_cycles : Waves.t -> int

  (** max no of wave cycles *)
  val get_max_signals : Waves.t -> int

  (** max width of wave window *)
  val get_max_wave_width : Waves.t -> int

  (** max height of wave window *)
  val get_max_wave_height : Waves.t -> int -> int

  (** draws one clock cycle *)
  val draw_clock_cycle
    :  ctx:G.ctx
    -> style:G.style
    -> bounds:Draw.rect
    -> w:int
    -> h:int
    -> c:int
    -> unit

  (** draws [cnt] clock cycles *)
  val draw_clock_cycles
    :  ctx:G.ctx
    -> style:G.style
    -> bounds:Draw.rect
    -> w:int
    -> waw:int
    -> h:int
    -> cnt:int
    -> unit

  (** draw binary waveform data *)
  val draw_binary_data
    :  ctx:G.ctx
    -> style:G.style
    -> bounds:Draw.rect
    -> w:int
    -> h:int
    -> data:Data.t
    -> off:int
    -> unit

  (** draw arbitrary waveform data *)
  val draw_data
    :  ctx:G.ctx
    -> style:G.style
    -> bounds:Draw.rect
    -> to_str:(Bits.t -> string)
    -> alignment:Wave_format.alignment
    -> w:int
    -> h:int
    -> data:Data.t
    -> off:int
    -> unit

  type 'a draw_item =
    ?style:Draw.Style.t -> ctx:G.ctx -> bounds:Draw.rect -> Waves.t -> 'a

  val with_border
    :  draw:'a draw_item
    -> label:string
    -> ?border:Draw.Style.t
    -> 'a draw_item

  (** draw cursor *)
  val draw_cursor : ctx:G.ctx -> bounds:Draw.rect -> state:Waves.t -> unit

  (** draw waveforms *)
  val draw_wave : unit draw_item

  (** draw signal names *)
  val draw_signals : unit draw_item

  (** draw signal values *)
  val draw_values : int draw_item

  val draw_status : unit draw_item

  (** draw standard user inferface (names, values, waveforms left to right *)
  val draw_ui : ?style:Styles.t -> ?bounds:Bounds.t -> ctx:G.ctx -> Waves.t -> unit

  type pick =
    | Wave of int * int
    | Value of int
    | Signal of int
    | Status
    | No_pick

  val pick : bounds:Bounds.t -> r:int -> c:int -> Waves.t -> pick
end

module Static : sig
  module R : module type of Make (Draw.In_memory)

  val draw
    :  ?signals:bool
    -> ?values:bool
    -> ?waves:bool
    -> ?style:Styles.t
    -> ?rows:int
    -> ?cols:int
    -> Waves.t
    -> Draw.In_memory.ctx

  val draw_full
    :  ?style:Styles.t
    -> Waves.t
    -> Draw.In_memory.ctx * Draw.In_memory.ctx * Draw.In_memory.ctx
end
