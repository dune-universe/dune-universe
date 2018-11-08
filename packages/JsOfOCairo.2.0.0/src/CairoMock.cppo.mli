(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = sig
  #include "S.incl.mli"
end

module Mock: sig
  include S

  val create: unit -> context
end

module Decorate(C: S): sig
  include S

  val create: C.context -> context

  val calls: context -> string list
end

include S

val create: unit -> context

val calls: context -> string list
