module Error : sig
  type t = AlreadySet
         | NotSquare

  module Exn : sig
    exception AlreadySet
    exception NotSquare
  end

  val to_exn : t -> exn

  val unwrap : ('a, t) result -> 'a
end

type t

val make : int -> int -> t

val get_inverted_matrix : t -> int array -> Matrix.t option

val insert_inverted_matrix : t -> int array -> Matrix.t -> (unit, Error.t) result
