module Hlist = Hlist

include (module type of Record_builder_intf) (** @inline *)

module Make(F : Partial_applicative_S) :
  Record_builder_S with type 'a applicative = 'a F.t

module Make_2(F : Partial_applicative_S2) :
  Record_builder_S2 with type ('a, 'e) applicative = ('a, 'e) F.t
