open Migrate_parsetree

val handle_error : (unit -> 'a) -> 'a
(** [handle_error f] executes [f ()] and if it raises exception
    it reports it and exit with status 2. *)

(** The functor to provide two PPX interfaces:

  - [register] registers the mapper, useful to build a preprocessor linked with
      other PPX mappers together.
  - [legacy_main] provides the classic standalone command line interfance
*)
module Make(A : sig
  val name : string
  val options : (Arg.key * Arg.spec * Arg.doc) list
  val make_mapper : Driver.config -> Driver.cookies -> Ast_405.Ast_mapper.mapper
end) : sig
  val register : unit -> unit
  val legacy_main : unit -> unit
end
