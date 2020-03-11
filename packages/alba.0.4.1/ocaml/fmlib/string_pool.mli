module IString:
  sig
    type t
    val id: t -> int
    val string: t -> string
    val compare: t -> t -> int
  end

type t
val intern: string -> t -> IString.t * t
val empty: t
