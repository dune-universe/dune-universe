type 'a t = private Zero | Item of 'a | Plus of 'a t * 'a t

val zero : 'a t

val of_item : 'a -> 'a t

val of_list : 'a list -> 'a t

val flatten : 'a t list -> 'a t

val plus : 'a t -> 'a t -> 'a t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val hd : 'a t -> 'a
     
class ['a] free_monoid :
  object ('a)
    constraint 'a = < plus : 'b t -> 'b t -> 'b t; zero : 'c t; .. >
    method plus : 'b t -> 'b t -> 'b t
    method zero : 'c t
  end
