(* include Nonstd *)
module String = Sosa.Native_string
module Option = Nonstd.Option
module List = Nonstd.List
module Array = Nonstd.Array
include Printf

module Float = struct
  include Nonstd.Float

  let ( + ) = ( +. )

  let ( - ) = ( -. )

  let ( * ) = ( *. )

  let ( / ) = ( /. )

  let pi = 4.0 * atan 1.

  let mid a b = (a + b) / 2.
end
