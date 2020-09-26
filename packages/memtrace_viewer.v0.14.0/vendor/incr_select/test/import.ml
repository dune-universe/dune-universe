include Expect_test_helpers_core
module Incr = struct
  module Incr = Incremental_debug.Make ()
  include Incr
  module Select = Incr_select.Make (Incr)
end
include Incr.Let_syntax

