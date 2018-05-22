
val register : ?applies_before:(Rule.t list) ->
               ?applies_after:(Rule.t list) ->
               Rule.t -> unit
(** Register a new rule. You may specify that this rule will apply
   before or after lists of rules. *)

val lookup : string -> Rule.t

val fold_by_priority : (Rule.t list -> 'a -> 'a) -> 'a -> 'a
