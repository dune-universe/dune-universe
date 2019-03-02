open! Import

module Config : sig
  type t =
    { mutable wave_width : int
    ; mutable wave_height : int
    ; mutable start_cycle : int
    ; mutable start_signal : int
    ; mutable wave_cursor : int
    ; mutable signal_cursor : int
    ; mutable signal_scroll : int
    ; mutable value_scroll : int
    }
  [@@deriving sexp_of]

  val default : t
end

type t =
  { cfg : Config.t
  ; waves : Wave.t array
  }
[@@deriving sexp_of]

val write : Out_channel.t -> t -> unit
val read : In_channel.t -> t
