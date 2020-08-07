module type ANY =
  sig
    type t
  end



module type SORTABLE =
  sig
    type t
    val compare: t -> t -> int
  end



module type FUNCTOR =
  sig
    type 'a t
    val return: 'a -> 'a t
    val map:    ('a -> 'b) -> 'a t -> 'b t
  end

module type APPLICATIVE =
  sig
    type 'a t
    val return: 'a -> 'a t
    val map:    ('a -> 'b) -> 'a t -> 'b t
    val (<*>):  ('a -> 'b) t -> 'a t -> 'b t
  end


module type MONAD =
  sig
    type 'a t
    val return: 'a -> 'a t
    val (>>=):  'a t -> ('a -> 'b t) -> 'b t
    val (>=>):  ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
    val map:  ('a -> 'b) -> 'a t -> 'b t
    val join: 'a t t -> 'a t
    val (<*>): ('a -> 'b) t -> 'a t -> 'b t
  end


module type READABLE =
  sig
    type t
    val has_more: t -> bool
    val peek: t -> char
    val advance: t -> t
  end




module type WRITABLE =
  sig
    type t
    val needs_more: t -> bool
    val put_character: t -> char ->  t
    val put_end: t -> t
  end
