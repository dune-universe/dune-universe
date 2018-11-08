(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

val drawing_tests: string list

module Make(X: sig
  val title: string

  module C: CairoMock.S

  module N: sig
    val name: string
    val create: unit -> C.context
    val backend: [`Cairo | `Node | `Browser | `CairoMock ]
  end

  module DrawingTest(T: sig
    type t = {name: string; width: int; height: int; draw: C.context -> unit}
  end): sig
    val run: T.t -> unit
  end

  module Limitation(L: sig
    type t = {name: string; width: int; height: int; draws: (C.context -> string list) list}
  end): sig
    val run: L.t -> unit
  end
end): sig
  val test: General.Testing.Test.t
end
