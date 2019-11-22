(** A [Display_rules.t] is an ordered list of rules that specifies the order of ports and
    the formatting of signals in a waveform.

    A port is displayed according to the first rule that it matches, using that rule's
    wave format.  Ports matching rules earlier in the list are displayed above ports
    matching rules later in the list. *)

open! Import

(** A [Rule.t] is a predicate on [Port.t]s that specifies the display format of matching
    ports. *)
module Rule : sig
  type t [@@deriving sexp_of]

  (** Default formatting - binary for 1 bit signals, hex otherwise. *)
  val default : t

  (** Use given [format] for ports whose name match the regular expression [re]. *)
  val port_name_matches : Re.re -> wave_format:Wave_format.t -> t

  (** Use [format] for ports with given name. *)
  val port_name_is
    :  ?alignment:Wave_format.alignment
    -> string
    -> wave_format:Wave_format.t
    -> t

  (** Match any one of a list of names. *)
  val port_name_is_one_of
    :  ?alignment:Wave_format.alignment
    -> wave_format:Wave_format.t
    -> string list
    -> t

  (** In [custom f], [f] returns [None] to signify no match, or [Some format] to specify a
      display format. *)
  val custom : f:(Port.t -> Wave_format.t option) -> t

  (** Similar tp [f], but allows the user to specify the alignment of the wave. *)
  val custom_with_alignment
    :  f:(Port.t -> (Wave_format.t * Wave_format.alignment) option)
    -> t
end

type t [@@deriving sexp_of]

val empty : t

(** [add_above t rule] returns rules where ports matching [rule] appear above ports
    matching the rules in [t]. *)
val add_above : t -> Rule.t -> t

(** [add_below t rule] returns rules where ports matching [rule] appear below ports
    matching the rules in [t]. *)
val add_below : t -> Rule.t -> t

val of_list : Rule.t list -> t

(** [combine ~above ~below] returns rules where ports matching the rules in [above]
    appear above ports matching the rules in [below]. *)
val combine : above:t -> below:t -> t

(** Construct the port order and formatting from the display rules and ports (derived from
    a testbench simulation object).  Unmatched ports are not shown, unless [Rule.default]
    (or a similar custom rule) is included as the last display rule. *)
val sort_ports_and_formats
  :  t
  -> Port.t list
  -> (Port.t * Wave_format.t * Wave_format.alignment) list

(** Check if a given port is displayed by any of the rules. *)
val is_displayed : t -> Port.t -> bool

(** Check if a given signal (treated as an internal port) is displayed by any of the rules. *)
val is_signal_displayed : t -> Hardcaml.Signal.t -> bool
