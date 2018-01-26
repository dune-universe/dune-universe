type assoc = Left | Right | Noassoc
type level = float

module Make (A : sig type t end) : sig
  type ppr = assoc -> level -> A.t
  type parens = A.t -> A.t
  val left : ppr -> ppr
  val right : ppr -> ppr
  val noassoc : ppr -> ppr
  val level : level -> ppr -> ppr
  val reset : ppr -> ppr
  val maybe_parens : parens -> assoc -> level -> ppr -> ppr
  val atom : A.t -> ppr
  val binop : parens -> (A.t -> A.t -> A.t) -> assoc -> level -> ppr -> ppr -> ppr
  val list : parens -> (A.t list -> A.t) -> level -> ppr list -> ppr
  val prefix : parens -> (A.t -> A.t) -> level -> ppr -> ppr
  val postfix : parens -> (A.t -> A.t) -> level -> ppr -> ppr
  val parens : parens -> ppr -> ppr
end
