type t = Idle | Working | Short_break | Long_break  (** State of the timer *)

val to_string : t -> string
(** [to_string t] produces the string representation of state [t] *)
