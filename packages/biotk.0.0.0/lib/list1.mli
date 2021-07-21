type 'a t = Cons of 'a * 'a list
val singleton : 'a -> 'a t
val cons : 'a -> 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val hd : 'a t -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t option
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'c -> f:('c -> 'a -> 'c) -> 'c
val of_list_exn : 'a list -> 'a t
val to_list : 'a t -> 'a list
val for_all : 'a t -> f:('a -> bool) -> bool
val exists : 'a t -> f:('a -> bool) -> bool
