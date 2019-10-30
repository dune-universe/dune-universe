open QCheck
module Alg = Alg_structs

(** The {!module-type:Alg.Functor.S} interface extended with a way of generating
    arbitrary {!module:QCheck} values.*)
module type S = sig
  include Alg.Functor.S
  val name : string
  val arbitrary : 'a arbitrary -> 'a t arbitrary
end

(** [test (module F)] is a list of {!module:QCheck} property based tests that
    checks whether the {{!module:Alg.Functor.Law} Functor Laws} hold for the
    given implementation [F]. *)
val test : (module S) -> Test.t list

(** [test implementations] is a flattened list of {!val:test}s
    generated for each implementation in implementations *)
val tests : (module S) list -> Test.t list
