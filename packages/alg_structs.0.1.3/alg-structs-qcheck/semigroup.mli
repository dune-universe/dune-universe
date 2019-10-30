open QCheck

module Alg = Alg_structs

(** The {!module-type:Alg.Semigroup.S} interface extended with a way of
    generating arbitrary {!module:QCheck} values. *)
module type S = sig
  include Alg.Semigroup.S
  val name : string
  val arbitrary : t arbitrary
end

(** [test impl_name (module S)] is a list of {!module:QCheck} property based tests that
    check whether the {{!module:Alg.Semigroup.Law} Semigroup Laws} hold for
    the given implementation [S]. *)
val test : (module S) -> Test.t list

(** [test implementations] is a flattened list of {!val:test}s
    generated for each implementation in implementations *)
val tests : (module S) list -> Test.t list
