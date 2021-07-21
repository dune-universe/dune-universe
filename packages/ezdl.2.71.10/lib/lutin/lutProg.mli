

type t

(**  [make infile mnode] *)
val make : ?libs:string list option -> string list -> string -> (t * Prog.t)

val get_init_state : ?verb_level:int -> t -> Prog.t -> Prog.state

(**  [make_state infile mnode] *)
val make_state : ?libs:string list option -> ?verb_level:int -> string list -> string
  -> Prog.state

(* val fomula_of : Var.env -> Var.env -> CoAlgExp.t -> Exp.formula  *)
