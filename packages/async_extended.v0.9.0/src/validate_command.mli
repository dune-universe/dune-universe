open! Core

module type S = sig
  type t [@@deriving of_sexp]
end

val create
  :  ?multiple_sexps_per_file:bool (* default false *)
  -> name:string
  -> (module S)
  -> Command.t
