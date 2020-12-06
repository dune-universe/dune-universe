(*Unfortunately Core.Int32 module does not define sexp, yojson, so we have to.*)
module CoreInt32_extended :
sig
  include (module type of Core.Int32)
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
end 
