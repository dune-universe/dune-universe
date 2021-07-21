include module type of Biocaml_unix.Range

val relative_position : t -> wrt:t -> [
    | `Before
    | `Before_with_intersection
    | `Included
    | `Equal
    | `Contains
    | `After_with_intersection
    | `After
  ]

val make : lo:int -> hi:int -> t

val convex_hull : t -> t -> t
