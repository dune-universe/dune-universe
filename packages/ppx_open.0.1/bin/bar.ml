module FooBar = struct
  type t =
    | A
    | B
    | C

  let to_string = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
end
