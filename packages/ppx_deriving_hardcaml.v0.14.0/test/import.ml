include (Base
         : (module type of struct include Base end
           with module Printf := Base.Printf))

include Stdio

include Expect_test_helpers_core

let string_of_int = Caml.string_of_int
