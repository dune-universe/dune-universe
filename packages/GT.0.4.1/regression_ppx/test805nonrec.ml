open GT

type 'a maybe = Just of 'a | Nothing [@@deriving gt ~gmap ~show ]

module P = struct
  type ('a,'b) p = 'a * 'b [@@deriving gt ~gmap ~show ]
end
open P
type nonrec ('a,'b) p = ('a,'b) p maybe [@@deriving gt ~gmap ~show ]

(* There is an issue with nonrec that when we will define a class
   we will not be able to see previous type `p`.
*)
module Test2 = struct
  open P
  type ('a,'b) p = ('a,'b) p maybe [@@deriving gt ~gmap ~show ]
end
