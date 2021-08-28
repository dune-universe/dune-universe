module Foo = struct
  let val1 = 1

  module type S = sig
    type z = Z
    type _ s = S

    type _ t =
      | Zero : z t
      | Succ : 'n t -> 'n s t
  end
end

{%%open| Foo.(val1 as renamed1, module type S as Nat) |}

let () = print_endline (string_of_int renamed1)

module Nat_impl : Nat = struct
  type z = Z
  type _ s = S

  type _ t =
    | Zero : z t
    | Succ : 'n t -> 'n s t
end

{%%open| Bar.FooBar.(type t (..) as bar1) |}

type foo = bar1 list

let x = A

let () =
  print_endline
    (Bar.FooBar.to_string x)