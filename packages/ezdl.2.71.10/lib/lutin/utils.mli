(** Annexe : utilitaires divers

----------------------------------------------------------*)

(**********************************************************)


(** iter_sep f s [x1;x2; ...;xn] est équivalent à :

	(f x1); s; (f x2); s; ... ; s; (f xn)
*)
val iter_sep : ('a -> unit) -> ('a -> unit) -> 'a list -> unit


val paranoid : unit -> bool
val set_paranoid : unit -> unit

val set_prof : unit -> unit
val time_C : string -> unit
val time_R : string -> unit
val time_P : unit -> unit
