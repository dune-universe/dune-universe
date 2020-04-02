include (module type of List)

val drop : int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val contains : 'a list -> 'a -> bool 
val substract : 'a list -> 'a list -> 'a list
(**  [substract] xs ys  removes the element that are in 
      both xs and ys from xs *)

val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> ('a list) * ('b list)
val to_string : ?sep:string -> 'a list -> ('a -> string) -> string


