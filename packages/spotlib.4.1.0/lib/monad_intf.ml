(** Minimum monad signature with an additional type parameter ['z]  *)
module type S2 = sig
  type (+'a, 'z) t
  val return : 'a -> ('a, 'z) t
  val bind : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
end

(** Infix name space for [S2] *)
module type Infix2 = sig
  type (+'a, 'z) t

  val ( >>= ) : ('a, 'z) t -> ('a -> ('b, 'z) t) -> ('b, 'z) t
  (** synonym of [bind] *)
    
  val ( >>| ) : ('a, 'z) t -> ('a -> 'b) -> ('b, 'z) t
  (** synonum of [fmap], with the flipped arguments *)

  (** Applicative style binops *)
    
  val (^<$>) : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t
    (** same as map, <$> in Haskell *)
    
  val (/<*>) : ('a -> 'b, 'z) t -> ('a, 'z) t -> ('b, 'z) t
  (** <*> in Haskell *)
end

(** Extension of [S2] *)
module type EX2 = sig
  type ('a, 'z) t

  val fmap : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t
  (** fmap in Haskell *)

  val liftM : ('a -> 'b) -> ('a, 'z) t -> ('b, 'z) t
  (** Synonym of [fmap] *)

  val fmap2 : ('a -> 'b -> 'c) -> ('a, 'z) t -> ('b, 'z) t -> ('c, 'z) t
  (** fmap2 in Haskell *)

  val liftM2 : ('a -> 'b -> 'c) -> ('a, 'z) t -> ('b, 'z) t -> ('c, 'z) t
  (** synonym of [fmap2] in Haskell *)

  val void : ('a, 'z) t -> (unit, 'z) t

  val seq : ('a, 'z) t list -> ('a list, 'z) t
  (** sequence in Haskell. Not tail recursive. *)

  val seq_ : (unit, 'z) t list -> (unit, 'z) t
  (** sequence_ in Haskell. Not tail recursive. *)

  val mapM : ('a -> ('b, 'z) t) -> 'a list -> ('b list, 'z) t
  (** Not tail recursive *)

  val mapM_ : ('a -> (unit, 'z) t) -> 'a list -> (unit, 'z) t
  (** Not tail recursive *)

  val iteri : (int -> 'a -> (unit, 'z) t) -> 'a list -> (unit, 'z) t
  (** Iteration with index starting from [0]. Not tail recursive. *)

  val for_ : int -> int -> (int -> (unit, 'z) t) -> (unit, 'z) t
  (** for like iteration. Not tail recursive *)

  val join : (('a, 'z) t, 'z) t -> ('a, 'z) t
end

(** The final unified Monad API for [S2] *)
module type T2 = sig
  include S2
  include EX2 with type ('a, 'z) t := ('a, 'z) t
  include Infix2 with type ('a, 'z) t := ('a, 'z) t

  module S : S2 with type ('a, 'z) t := ('a, 'z) t
  module EX : EX2 with type ('a, 'z) t := ('a, 'z) t
  module Infix : Infix2 with type ('a, 'z) t := ('a, 'z) t
end

module type S1 = sig
  type +'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Infix name space for [S1] *)
module type Infix1 = sig
  type +'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** synonym of [bind] *)
    
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  (** synonum of [fmap], with the flipped arguments *)

  (** Applicative style binops *)
    
  val (^<$>) : ('a -> 'b) -> 'a t -> 'b t
    (** same as map, <$> in Haskell *)
    
  val (/<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** <*> in Haskell *)
end

(** Extension of [S1] *)
module type EX1 = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
  (** fmap in Haskell *)

  val liftM : ('a -> 'b) -> 'a t -> 'b t
  (** synonym of [fmap] *)

  val fmap2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** fmap2 in Haskell *)

  val liftM2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** synonym of [fmap2] *)

  val void : 'a t -> unit t

  val seq : 'a t list -> 'a list t
  (** sequence in Haskell. Not tail recursive. *)

  val seq_ : unit t list -> unit t
  (** sequence_ in Haskell. Not tail recursive. *)

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  (** Not tail recursive *)

  val mapM_ : ('a -> unit t) -> 'a list -> unit t
  (** Not tail recursive *)

  val iteri : (int -> 'a -> unit t) -> 'a list -> unit t
  (** Iteration with index starting from [0]. Not tail recursive. *)

  val for_ : int -> int -> (int -> unit t) -> unit t
  (** for like iteration. Not tail recursive *)

  val join : 'a t t -> 'a t
 end

(** The final unified Monad API for [S1] *)
module type T1 = sig
  include S1
  include EX1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t

  module S : S1 with type 'a t := 'a t
  module EX : EX1 with type 'a t := 'a t
  module Infix : Infix1 with type 'a t := 'a t
end

module type S = S1
module type T = T1
module type EX = EX1
module type Infix = Infix1
