

val to_lp : Value.Env.t -> Ast.file -> Lp.t
(** [to_lp env ast] evaluate [ast] in the environment [env] and
   generates an output lp file. raise exception [Error] *)


(** Error reporting *)

type error

exception Error of error * Loc.t

val report_error : Format.formatter -> error -> unit
