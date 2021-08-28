module Foo =
  struct
    type 'a t =
      | Foo of 'a 
    let val1 = Foo 1
    let val2 = Foo 2
    module type S  = sig type 'a t val val1 : int t val val2 : int t end
    module Bar =
      struct
        type t =
          | A 
          | B 
          | C 
        let to_string = function | A -> "A" | B -> "B" | C -> "C"
      end
  end
open struct let val1 = Foo.val1 end
open struct let val1 = Foo.val1
            let renamed1 = Foo.val2 end
open struct module Bar = Foo.Bar
            module B = Foo.Bar end
open struct module type S  = Foo.S
            module type Foo_S  = Foo.S end
