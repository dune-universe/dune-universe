val term : int Cmdliner.Term.t

val handle_errors : (unit, Omigrate.Error.t) Result.t -> int

val database_arg : string Cmdliner.Term.t

val source_arg : string Cmdliner.Term.t

val envs : Cmdliner.Term.env_info list

val exits : Cmdliner.Term.exit_info list

module Let_syntax : sig
  val ( let+ ) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t

  val ( and+ )
    :  'a Cmdliner.Term.t
    -> 'b Cmdliner.Term.t
    -> ('a * 'b) Cmdliner.Term.t
end
