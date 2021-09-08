open Diagram
 
type 'a t = T of 'a * ('a t) list

val map : ('a -> 'b) -> 'a t -> 'b t

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val singleton : 'a -> 'a t

val to_diagram : ?hgap : float -> ?vgap : float -> diagram t -> diagram
