type t

module Style : sig
  type t =
    | Utf
    | Ascii
    | Line
    | Circle
    | Braille
    | Braille_spin
    | Vertical
end

module Options : sig
  type t =
    { style : Style.t
    ; total_width : int option (* When not set, use the terminal width *)
    ; prefix : string
    }

  val default : t
end

(** [create ?options total] creates a new progress bar tied to stdout. The bar reaches
    100% when [update] is called with [total].
*)
val create : ?options:Options.t -> int -> t

(** [reset t] resets the current bar. *)
val reset : t -> unit

(** [update t v] sets the progress bar a value [v], the proportion of the bar displayed
    is [v / total] where [total] is the value that has been used to initialize the bar.
    The bar is then displayed.
*)
val update : t -> int -> unit

(** [incr t ~by] increases the current value stored in [t] by [by]. *)
val incr : t -> by:int -> unit

(** [close t] finalizes the bar and prints a new line. *)
val close : t -> unit

(** [with_bar total ~f] creates a new bar and runs [f] using it. [close] is called when
    [f] returns or if [f] raises.
*)
val with_bar : ?options:Options.t -> int -> f:(t -> 'a) -> 'a
