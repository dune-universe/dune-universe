(* Time-stamp: <modified the 14/03/2013 (at 14:18) by Erwan Jahier> *)


(** Returns a predef operator interpreter. Raises Not_found if the
    operator is not defined.
*)
val get: Soc.key -> (SocExecValue.ctx -> SocExecValue.ctx)
