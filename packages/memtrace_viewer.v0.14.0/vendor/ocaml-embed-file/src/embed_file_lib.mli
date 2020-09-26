open! Core
open! Async

val chunk_len : int
val chunks : string -> string list
val replace_CRs : string -> string
val write_ml : Writer.t -> var:string -> contents:string -> unit
val write_mli : Writer.t -> var:string -> unit
val command : Command.t

module Private : sig
  val variable_name_of_file_name : string -> string
end
