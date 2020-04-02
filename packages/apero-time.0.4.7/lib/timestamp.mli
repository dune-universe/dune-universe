open Apero
open Time

module Timestamp : sig
  
  module type S = sig
    module Time: Time

    module T: sig
      type t
      val compare: t -> t -> int
      val equal: t -> t -> bool
    end

    include (module type of Ordered.Make (T))

    val create: Uuid.t -> Time.t -> t
    val get_source: t -> Uuid.t
    val get_time: t -> Time.t

    val to_string: t -> string
    val of_string: string -> t option

    val encode: t -> Abuf.t -> unit
    val decode: Abuf.t -> t

    val pp: Format.formatter -> t -> unit
end

  module Make (T: Time) : S with module Time = T

end
