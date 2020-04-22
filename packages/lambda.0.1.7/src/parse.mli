exception InvalidLambdaString
val parse: string -> L.term
(** [parse s] converts s to a term, or raise InvalidLambdaString on failures *)