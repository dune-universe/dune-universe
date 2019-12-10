val format_rate : float -> string
module Time : sig
  module Span : sig
    type t
    val format : t -> string
    val of_secs : float -> t
    val to_secs : t -> float
  end
  type t
  val now : unit -> t
  val diff : t -> t -> Span.t
end
