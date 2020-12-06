(*Unfortunately Core.Int64 module does not define yojson, sexp converters, so we have to.*)
module CoreInt64_extended :
sig
  include (module type of Core.Int64)
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
end 
