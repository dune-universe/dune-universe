open QCheck
module Alg = Alg_structs

(** The {!module-type:Alg.Applicative.S} interface extended with a way of
    generating arbitrary {!module:QCheck} values for {!type:S.t}. *)
module type S = sig
  include Alg.Applicative.S
  val name : string
  val arbitrary : 'a arbitrary -> 'a t arbitrary
end

(** [test (module A)] is a list of {!module:QCheck} property based tests that
    checks whether the {{!module:Alg.Applicative.Law} Applicative Laws} hold for
    the given implementation [A]. *)
val test : (module S) -> Test.t list

(** [test implementations] is a flattened list of {!val:test}s
    generated for each implementation in [implementations] *)
val tests : (module S) list -> Test.t list
