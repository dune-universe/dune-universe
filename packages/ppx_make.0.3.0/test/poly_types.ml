type 'a poly_opt = 'a option [@@deriving make]

type 'a poly_tuple = 'a * int [@@deriving make]

type ('a, 'b) poly_rec = { r_a : 'a; r_b : 'b; r_int : int } [@@deriving make]

type ('a, 'b) poly_var = Ok of 'a | Error of 'b | Other [@@deriving make]
