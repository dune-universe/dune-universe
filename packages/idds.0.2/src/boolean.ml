module type Algebra = sig
  val declare_var : string -> [`Ok | `Duplicate]

  type t
  val fls : t
  val tru : t
  val of_bool : bool -> t
  val var : string -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val ( ! ) : t -> t
  val ( == ) : t -> t -> bool
end

