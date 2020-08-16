open Httpaf

type media_type = Any | Any_sub_type of string | Type of string * string

val extract : Headers.t -> (media_type * float) list
