include struct
  open Cohttp_async
  module Body = Body
  module Request = Request
  module Response = Response
  module Server = Server
end

include struct
  open Cohttp
  module Code = Code
end
