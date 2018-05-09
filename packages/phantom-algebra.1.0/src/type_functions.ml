(** Type-level functions

    Skim through this section when reading the documentation
    for the first (second, third and …) time
*)

(** {2 Small integer type representation } *)

type 'a z = [`zero of 'a]
type 'a one = [`one of 'a]
type 'a two = [`two of 'a]
type 'a three = [`three of 'a]
type 'a four = [`four of 'a]

type ('a,'b,'c) any =
  [< `zero of 'b & 'a | `one of 'b & 'a | `two of 'b & 'a] as 'c

(** {2 Type-level functions} *)

(**
    [(x,y,z,d1,d2,d3_ ) product] computes the types of (x,d1) * (y,d2= and
    put the result inside z and d3.
    In practice, the aims is to direct the unification of the type variables
    using the type values of the inputs.
    For the product we have the following types
    [(''dim1, 'rank1) t -> ('dim2,'rank2) t -> ('dim3, 'rank3) t ]
    and we want to unify ['rank3] and ['dim3] with the right values
*)
type ('rank1, 'rank2,'rank3,'dim1,'dim2,'dim3, 'parameters) product =
  [<`zero of 'rank2 & (* if rank1 is zero: scalar broadcasting *)
             [< `zero of 'rank3 * 'dim3 & 'p1 z * 'p2 one
             (* scalar * scalar ⇒ scalar *)
             | `one of 'rank3 * 'dim3 & 'p1 one * 'dim2
             (* scalar * vector('dim) ⇒ vector('dim) *)
             | `two of 'rank3 * 'dim3 & 'p1 two * 'dim2
             (* scalar * matrix('dim) ⇒ matrix('dim) *)
             ]
  | `one of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 one * 'dim2
            (* vector('dim) * scalar ⇒ vector('dim) *)
            | `one of 'rank3 * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            (* vector('dim) * vector('dim) ⇒ vector('dim) *)
            | `two of 'rank3 * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            (* vector('dim) * matrix('dim) ⇒ vector('dim) *)
            ]
  | `two of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 two * 'dim2
            (* matrix('dim) * scalar⇒ matrix('dim) *)
            | `one of 'rank3 * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            (* matrix('dim) * vector('dim)⇒ vector('dim) *)
            | `two of 'rank3 * 'dim2 * 'dim3 & 'p1 two * 'dim1 * 'dim1
              (* matrix('dim) * matrix('dim)⇒ matrix('dim) *)
            ]
  ] as 'rank1
  constraint 'parameters = 'p1 * 'p2 * 'p3


type ('rank1, 'rank2,'rank3,'dim1,'dim2,'dim3, 'parameters) div =
  [<`zero of 'rank2 & (* scalar broadcasting *)
             [< `zero of 'rank3 * 'dim3  & 'p1 z * 'p2 one]
  | `one of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 one * 'dim1
            | `one of 'rank3 * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            | `two of 'rank3 * 'dim2 * 'dim3 & 'p1 one * 'dim1 * 'dim1
            ]
  | `two of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 two * 'dim1
            | `two of 'rank3 * 'dim2 * 'dim3 & 'p2 two * 'dim1 * 'dim1
            ]
  ] as 'rank1
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) div computes the rank of x * y and
    put the result inside z *)

type ('rank1, 'rank2,'rank3, 'parameters) rank_diff =
  [<
    | `one of 'rank2 & [< `one of 'rank3 & 'p1 z]
    | `two of 'rank2 &
              [< `one of 'rank3 & 'p1 one
              | `two of 'rank3 & 'p1 z ]
  ] as 'rank1
  constraint 'parameters = 'p1
(** (x,y,z,_ ) diff computes the rank of x - y and
    put the result inside z *)


type ('rank1, 'rank2,'rank3,'dim1,'dim2,'dim3, 'parameters) sum =
  [<`zero of 'rank2 & (* scalar broadcasting *)
             [< `zero of 'rank3 * 'dim3 & 'p1 z * 'p2 one
             | `one of 'rank3 * 'dim3 & 'p1 one * 'dim2
             | `two of 'rank3 * 'dim3 & 'p1 two * 'dim2]
  | `one of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 one * 'dim1
            | `one of 'rank3 * 'dim1 * 'dim3 & 'p1 one * 'dim2 * 'dim2 ]
  | `two of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 two * 'dim1
            | `two of 'rank3 * 'dim1 * 'dim3 & 'p1 two * 'dim1 * 'dim3 ]
  ] as 'rank1
  constraint 'parameters = 'p1 * 'p2 * 'p3
(** (x,y,z,_ ) sum computes the rank of x + y and
    put the result inside z *)


type ( 'dim, 'res, 'parameters ) cross =
  [< `two of 'res & ('p2 * 'p1 z) | `three of 'res & ('p2 three * 'p1 one) ]
  as 'dim
  constraint 'parameters = 'p1 * 'p2


type ('dim1,'dim2,'dim3,'p) simple_sum =
    [< `one of 'dim2 &
               [< `one of 'dim3 & 'p two
               | `two of 'dim3 & 'p three
               | `three of 'dim3 & 'p four
               ]
    | `two of 'dim2 &
              [< `one of 'dim3 & 'p three
              | `two of 'dim3 & 'p four
              ]
    | `three of 'dim2 & [< `one of 'dim3 & 'p four]
    ] as 'dim1

(* dim1 + dim2 = dim3 *)
type ('dim1,'dim2,'dim3,'p) nat_sum =
  [< `one of 'dim2 &
             [< `one of 'dim3 & 'p two
             | `two of 'dim3 & 'p three
             | `three of 'dim3 & 'p four ]
  | `two of 'dim2 &
             [< `one of 'dim3 & 'p three
             | `two of 'dim3 & 'p four ]
  | `three of 'dim2 &
              [< `one of 'dim3 & 'p four ]
  ]
  as 'dim1

(* (dim + rank) (dim + len + rank ) + (dim,rank) *)
type ('tensor_rank,'index_rank,'res_rank,'dim,'res_dim, 'len, 'parameters)
    superindexing =
  [< `two of
        'index_rank &
        [< `two of (* m.{xx&xy} *)
             'len &
             [< `one of 'res_rank & 'p z
             | `two of 'res_dim * 'res_rank & 's two * 'p one
             | `three of 'res_dim * 'res_rank & 's three * 'p one
             | `four of 'res_dim * 'res_rank & 's four * 'p one
             ]
        | `one of (* m.{x} or m.{x&…&x_k} with k = dim m ⇒ dim = dim m *)
             'dim * 'len &
             'res_dim *
             [< `one of 'res_rank & 'p one (*m.{x} ⇒ rank=1, dim=dim m)*)
               (* vv  m.{x&…xy} ⇒ rank=2, dim = dim m vv*)
             | `two of 'dim * 'res_rank  & 'p two * 's two
             | `three of 'dim * 'res_rank  & 'p three * 's two
             | `four of 'dim * 'res_rank  & 'p four * 's two
             ]
        ]
    | `one of 'len &
              [< `one of 'res_rank & 'p z (* v.{x}: scalar *)
              | `two of 'res_rank * 'res_dim & 'p one * 's two
              | `three of 'res_rank * 'res_dim & 'p one * 's three
              | `four of 'res_rank * 'res_dim & 'p one * 's four
              ]
        ] as 'tensor_rank
      constraint 'parameters = 'p * 's
