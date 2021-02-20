open GT

type 'a list = Cons of 'a * 'a list | Nil [@@deriving gt ~gmap ~show ]
type t = [ `A | `B ] list
[@@deriving gt ~gmap ~show ]


(* type ('a,'b) demo = 'a * 'b *)
(* type nonrec ('a,'b) demo = ('a,'b) demo list *)
