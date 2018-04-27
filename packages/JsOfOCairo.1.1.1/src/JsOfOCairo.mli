(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

module type S = CairoMock.S

include S

val create: Dom_html.canvasElement Js.t -> context
