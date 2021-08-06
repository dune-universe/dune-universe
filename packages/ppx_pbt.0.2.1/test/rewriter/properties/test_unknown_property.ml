let my_property f x y = f x y = x + y

let add x y = x + y [@@pbt {| my_property[int, int] |}]
