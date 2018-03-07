module PPStyle = PrettyPrint.Style

module Index = Index.Make(Base.Int)

let id = Index.id

let ( ?? ) = id

module Literal = Literal.Make(Index)

module Clause = Clause.Make(Literal)

module Term = Term.Make(Literal)

module CNF = CNF.Make(Clause)

module DNF = DNF.Make(Term)

module NNF = NNF.Make(Literal)

module UnNF = UnNF.Make(Literal)