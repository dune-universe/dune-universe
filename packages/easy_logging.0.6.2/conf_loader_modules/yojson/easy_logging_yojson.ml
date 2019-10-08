
open Easy_logging_yojson_aux



module Handlers = Handlers
module Logging = MakeLogging(Handlers)

module Logging_internals =
struct

  module Logging_types = Logging_types
  module Formatters = Easy_logging.Logging_internals.Formatters
  module MakeLogging = MakeLogging
  module Logging_infra = Easy_logging.Logging_internals.Logging_infra
  module Colorize = Easy_logging.Logging_internals.Colorize
end
