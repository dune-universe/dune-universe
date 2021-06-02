module type IO = Io.IO

module type Parser = sig
  module IO : IO
  module Compliance : Compliance.S

  val decode
    :  reader : (unit -> Tokens.token IO.t)
    -> (Compliance.json option, string) result IO.t
end

module Make (Compliance : Compliance.S) (IO : IO) : Parser
  with module IO = IO
   and module Compliance = Compliance
