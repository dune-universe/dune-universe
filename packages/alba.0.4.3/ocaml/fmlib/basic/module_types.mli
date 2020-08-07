(** Common module types like [ANY, SORTABLE, ...] *)




(** Can be any type. *)
module type ANY =
  sig
    type t
  end




(** A sortable type is a type with a comparison function. *)
module type SORTABLE =
  sig
    type t
    val compare: t -> t -> int
  end






(** A functor is an abstract container which is mappable *)
module type FUNCTOR =
  sig
    (** Type of the abstract container containing values of type ['a].*)
    type 'a t

    (** [return a] makes an abstract container containing the value [a]. *)
    val return: 'a -> 'a t

    (** [map f m] extracts the value [a] from the abstract container and
       returns a container containing [f a]. *)
    val map:    ('a -> 'b) -> 'a t -> 'b t
  end



(** An applicative functor is an abstract container which is mapable and if it
    has functions in it the functios can be applied. *)
module type APPLICATIVE =
  sig
    (** Type of the applicative container. *)
    type 'a t

    (** [return a] makes an applicative container containing the value [a]. *)
    val return: 'a -> 'a t

    (** [map f m] extracts the value [a] from the abstract container and
       returns a container containing [f a]. *)
    val map:    ('a -> 'b) -> 'a t -> 'b t

    (** [f <*> m] applies the function contained in [f] to the content of the
       applicative container [m] and injects the result into a new applicative
       container. *)
    val (<*>):  ('a -> 'b) t -> 'a t -> 'b t
  end



(** A monad is an applicative functor with a bind [>>=] operator. *)
module type MONAD =
  sig

    (** The type of the monad containing values of type ['a] *)
    type 'a t

    (** [return a] makes a monadic container containing the value [a]. *)
    val return: 'a -> 'a t


    (** [m >>= f] extracts the value [a] from the monadic container [m] and
       returns [f a]. *)
    val (>>=):  'a t -> ('a -> 'b t) -> 'b t


    (** [f >=> g] composition of the two monadic functions [f] and [g].

        [f >=> g] is equivalent to [fun a -> f a >>= g].

     *)
    val (>=>):  ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)


    (** [map f m] maps the values in the monadic container [m] with the
       function [f]. *)
    val map:  ('a -> 'b) -> 'a t -> 'b t


    (** Remove one level of container. [join m] is equivalent to [mm >>= fun m
       -> m]. *)
    val join: 'a t t -> 'a t


    (** [f <*> m] applies the function contained in [f] to the content of the
       monadic container [m] and injects the result into a new monadic
       container. *)
    val (<*>): ('a -> 'b) t -> 'a t -> 'b t
  end


(** Readable structure *)
module type READABLE =
  sig
    (** Type of the structure.*)
    type t

    (** Does the structure have more characters to read? *)
    val has_more: t -> bool

    (** [peek r] returns the next character. *)
    val peek: t -> char

    (** [advance r] advances the structure by one character. *)
    val advance: t -> t
  end




(** Writable structure *)
module type WRITABLE =
  sig
    (** Type of the structure.*)
    type t

    (** Is it possible to write more characters to the structure? *)
    val needs_more: t -> bool

    (** [put_character w c] writes the character [c] to the structure and
    returns a structure which might accept more characters. *)
    val put_character: t -> char ->  t

    (** [put_end w] signals to the structure [w] that there are no more
       characters available to write (e.g. eof reached). *)
    val put_end: t -> t
  end
