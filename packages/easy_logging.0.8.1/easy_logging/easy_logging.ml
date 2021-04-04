(*
    This file is part of easy_logging.

    easy_logging is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    easy_logging is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with easy_logging.  If not, see <https://www.gnu.org/licenses/>.
*)


module Handlers = Handlers
module Logging = Logging

module Logging_internals =
struct

  module Formatters = Formatters
  module Logging_types = Logging_types
  module Logging_infra = Logging_infra
  module Colorize = Colorize
end
