module type Parser = sig
  module Compliance : Compliance.S
  type t

  val create :  reader : (unit -> Tokens.token) -> t
  val decode : t -> (Compliance.json_stream option, string) result
end

module Make (Compliance : Compliance.S) : Parser
  with module Compliance = Compliance

