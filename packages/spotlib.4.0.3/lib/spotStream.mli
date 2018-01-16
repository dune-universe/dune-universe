(** Lazy list or Stream *)

(** {6 Type } *)

type 'a desc = 
  | Cons of 'a * 'a desc lazy_t
  | Null 

type 'a t = 'a desc lazy_t

(** {6 Constructors} *)

val null : 'a t
(** a constant null *)

val cons : 'a -> 'a t -> 'a t
val (^^) :  'a -> 'a t -> 'a t
(** same as [cons] *)

val singleton : 'a -> 'a t

val create : ('st -> ('a * 'st) option) -> 'st -> 'a t
(** Pure functional creator *)

(** {6 Deconstructors} *)

val hd : 'a t -> 'a
val tl : 'a t -> 'a t

val peek : 'a t -> ('a * 'a t) option
(** You can use match with lazy pattern instead of [peek] *)

val is_null : 'a t -> bool
(** null check *)

val nth : 'a t -> int -> 'a

val length : 'a t -> int

(** {6 Conversions between the eager list} *)

val to_list : 'a t -> 'a list
(** Conversion from a lazy stream to a strict list.
    Do not use against inifinite streams. *)

val of_list : 'a list -> 'a t
(** Conversion from a strict list to a lazy stream. 
    The conversion itself is done strictly: the result has no reference to the original list *)

(** {6 Misc functions} *)

val take : int -> 'a t -> 'a t
(** [take n t] takes the first [n] elements of [t]. Lazy. *)

val init : 'a t -> 'a t
(** [init t] returns the same list [t] but without its final element. *)

val rev : 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val concat : 'a t t -> 'a t

val mem : 'a -> 'a t -> bool
(** Membership. Never terminates over inifinite streams. *)
  
val iter : ('a -> unit) -> 'a t -> unit
(** Iteration. Never terminates over inifinite streams. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map *)

val fold_left : ('a Lazy.t -> 'b -> 'a Lazy.t) -> 'a Lazy.t -> 'b t -> 'a Lazy.t
(** Folding left *)

val fold_left1 : ('a Lazy.t -> 'a Lazy.t -> 'a Lazy.t) -> 'a Lazy.t t -> 'a Lazy.t
(** [fold_left1 f (x^^xs) = fold_left f x xs] 
    [fold_left1 f null] raises an exception.
*) 

val fold_left' : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
(** Folding left, strict version. Never terminates over inifinite streams. Tail recursive. *) 

val fold_right : ('a -> 'b lazy_t -> 'b lazy_t) -> 'a t -> 'b lazy_t -> 'b lazy_t
(** [fold_right f t init]. Folding right. Not tail recursive. 
    If laziness is required, it is the responsibility of [f]. 
*)

val fold_right1 : ('a -> 'a lazy_t -> 'a lazy_t) -> 'a t -> 'a lazy_t -> 'a lazy_t
(** [fold_right1 f (append xs (singleton x)) = fold_right f xs x] 
    [fold_right1 f null] raises an exception.
    If laziness is required, it is the responsibility of [f]. 
*)

val append : 'a t -> 'a t -> 'a t
(** Append. Lazy. *)

val intercalate : 'a t -> 'a t t -> 'a t
(** Haskell's intercalate *)

val intersparse : 'a -> 'a t -> 'a t
(** Haskell's intersparse *)

val split_at : int -> 'a t -> 'a list * 'a t
(** Haskell's splitAt. Prefix is forced. *)

val split_at' : int -> 'a t -> 'a t * 'a t
(** Haskell's splitAt. Prefix is lazy. *)

(** {6 Inpure hacks} *)

val rev_between : 'a t -> 'a t -> 'a list
val between : 'a t -> 'a t -> 'a list
(** Get elements between two points of the *same* stream.

    The first argument of [rev_between] and [between] must point the former element of the stream
    than the second argument. Otherwise, the behaviour of [rev_between] and [between] are not specified:
    a wrong result or memory exhaustion by an infinite loop.
*)

(** {6 Monadic interface} *)
include Monad.T with type 'a t := 'a t
