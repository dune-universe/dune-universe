[@@@SCaml iml_optimization=false]
open SCaml
let [@entry] main x y =
  [],
  assert ( 
    Crypto.check_signature 
      (Key "edpkuSR6ywqsk17myFVRcw2eXhVib2MeLc9D1QkEQb98ctWUBwSJpF")
      (Signature "edsigu4chLHh7rDAUxHyifHYTJyuS8zybSSFQ5eSXydXD7PWtHXrpeS19ds3hA587p5JNjyJcZbLx8QtemuJBEFkLyzjAhTjjta")
      (Bytes "68656c6c6f" (* hello *))
  )
