
module Json : sig

  type t 

  val of_string : string -> t
  val to_string : t -> string
  val validate : string -> (t, Atypes.error) Acommon.Result.t


end
