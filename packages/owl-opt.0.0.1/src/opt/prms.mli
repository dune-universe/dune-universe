(** 
  Example:
  {[
   module Prms = struct
      type 'a t = {a: 'a; b: 'a} [@@deriving prms]
   end
  ]}
  [Prms] has type [PT]. The user defines the desired paramter record ['a t] and [[@@deriving prms]] derives the corresponding functions map, map2, iter, and iter2.
 *)

(** input module type to functors that make optimisation modules such as {!module:Owl_opt.D.Adam.Make} *)
module type PT = sig
  type 'a t

  val map : f:('a -> 'b) -> 'a t -> 'b t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val iter : f:('a -> unit) -> 'a t -> unit
  val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
end

(** module for single paramter [a] *)
module Single : sig
  include PT

  val unpack : 'a t -> 'a
  val pack : 'a -> 'a t
end

(** module for pair paramters [(a1,a2)] *)
module Pair : sig
  include PT

  val unpack : 'a t -> 'a * 'a
  val pack : 'a * 'a -> 'a t
end
