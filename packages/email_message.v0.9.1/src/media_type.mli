open! Core

type t

val multipart_boundary : t -> Boundary.t option
val last : Headers.t -> t option
val default : parent:(t option) -> t
