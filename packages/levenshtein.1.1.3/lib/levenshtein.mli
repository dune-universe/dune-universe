(**

   Levenshtein distance algorithm for general array.

   Author: jun.furuse@gmail.com
   License: public domain

*)

(** Generic array type. 

    [get] does not need to check the range. 

    You can use Array or String with overriding [get] 
    with [Array.unsafe_get] or [String.unsafe_get] respectively, i.e.:

      module A = struct
        type elem = <YOUR ELEMENT TYPE>
        type t = elem array
        let compare = ...
        let get = unsafe_get
        let size = Array.size
      end

      module A' = struct
        type elem = char
        type t = string
        let compare (c1 : char) c2 = compare c1 c2
        let get = String.unsafe_get
        let size = String.length
      end
*)
module type Array = sig
  type t   (** Type of arrays *)

  type elem (** Type of the elements of arrays *)

  val compare : elem -> elem -> int

  val get : t -> int -> elem (** no need of range checking *)

  val size : t -> int
end

(** Simple implementation without cache *)
module type S = sig
  type t
  val distance : ?upper_bound: int -> t -> t -> int
  (** Calculate Levenshtein distance between two [t]'s.
      
      If we are only interested in the distance if it is smaller than 
      some threshold, specifying [upper_bound] greatly improves the performance.
      In that case, the return value is always is culled to [upper_bound].
  *)
end

module Make(A : Array) : S with type t = A.t

module String : S with type t = string

(** Cached result *)
type result = 
  | Exact of int
  | GEQ of int (** Cache knows the result is greater than or equal to this value *)

(** Cache *)
module type Cache = sig
  type 'a t
  type key
  val create : int -> 'a t
  val alter : 'a t -> key -> ('a option -> 'a option) -> 'a option
end

(** Build a cache using OCaml stdlib's Hashtbl *)
module CacheByHashtbl(H : Hashtbl.HashedType) 
  : Cache with type key = H.t

module type WithCache = sig
  type t
  type cache
  val create_cache : int -> cache
  val distance : cache -> ?upper_bound:int -> t -> t -> result
end

module MakeWithCache(A : Array)(C : Cache with type key = A.t * A.t) 
  : WithCache with type t = A.t
              and  type cache = result C.t

(** Sample implementation for String, using OCaml stdlib's Hashtbl as a cache *)
module StringWithHashtbl : WithCache with type t = string
