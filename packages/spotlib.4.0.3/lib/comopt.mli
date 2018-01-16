(*

  Short name. ex. -x. Always one char.

  Long name. ex. --baa. Always two -s.

  Arity. Null or Unary.

  Anonymous args
 
*)

type ('a, 'err) opt

module Error : sig
  type t =
    [ `Ambiguous              of string * string * string
    | `Nullary_takes_argument of string
    | `Requires_argument      of string
    | `Unknown                of string 
    ]

  val to_string : t -> string
end

val nullary : char option -> string option -> 'a -> ('a, 'err) opt

val unary : char option -> string option -> (string -> ('a, 'err) result) -> ('a, 'err) opt

val parse : ([> `Anon of string ] as 'a,
             [> `Ambiguous of string * string * string
             |  `Requires_argument of string
             |  `Nullary_takes_argument of string
             |  `Unknown of string ] as 'err) opt list 
          -> string list 
          -> ('a list, 'err) result
