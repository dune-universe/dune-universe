module Foo = struct
  type 'a t = Foo of 'a

  let val1 = Foo 1
  let val2 = Foo 2

  module type S = sig
    type 'a t

    val val1 : int t
    val val2 : int t
  end

  module Bar = struct
    type t =
      | A
      | B
      | C

    let to_string = function
      | A -> "A"
      | B -> "B"
      | C -> "C"
  end
end

[%%open {| Foo.(val1) |}]
[%%open {| Foo.(val1, val2 as renamed1) |}]
[%%open {| Foo.(module Bar, module Bar as B) |}]
[%%open {| Foo.(module type S, module type S as Foo_S) |}]

(* [%%open {| Foo.(type t as foo_t) |} ]

[%%open {| Foo.(type t (..) as foo_t) |} ] *)
