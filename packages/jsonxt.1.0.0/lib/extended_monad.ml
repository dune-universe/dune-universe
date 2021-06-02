module type IO = Io.IO

module Make (IO : IO) = struct
  module Parser = Parser_monad.Make (Extended.Compliance) (IO)
  include Reader_monad.Make (Parser)
  include Writer_monad.Make (Extended.Compliance) (IO)
end
