
type env

val empty : string -> string list -> (string * Int64.t) list -> env

module type F = sig
  val exists : string -> bool
  val load : string -> string
end

module type S = sig
  include F
  val preprocess: env -> string -> string -> string * env
end

module Make(F:F) : S 

module FileSystem : S
