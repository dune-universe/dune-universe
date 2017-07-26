include Expect_test_helpers_kernel
module Incr = struct
  module Incr = Incremental_kernel_debug.Incremental.Make ()
  include Incr
  module Select = Incr_select.Make (Incr)
end
include Incr.Let_syntax

