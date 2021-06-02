module type Parser = sig
  module Compliance : Compliance.S

  val decode
    :  reader : (unit -> Tokens.token)
    -> (Compliance.json option, string) result
end

module Make (Compliance : Compliance.S) : Parser
  with module Compliance = Compliance
