open Biocaml_unix

type t = private Range.t list

val empty : t

val singleton : Range.t -> t

val add : t -> Range.t -> t

val diff_range : t -> Range.t -> t
