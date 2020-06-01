module type S = sig
  val dir_directory : string -> unit
  val dir_install_printer : Format.formatter -> Longident.t -> unit
end

val unsafe_string : unit -> bool
val init : native:bool -> (module S) -> unit
