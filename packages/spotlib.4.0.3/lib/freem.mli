module M1 : sig
  type +'a t =
    | Return : 'a -> 'a t
    | Bind   : 'a t * ('a -> 'b t) -> 'b t
  include Monad.T with type 'a t := 'a t
end

include module type of M1
    
module M2 : sig
  type (+'a, 'z) t =
    | Return : 'a -> ('a, 'z) t
    | Bind   : ('a, 'z) t * ('a -> ('b, 'z) t) -> ('b, 'z) t
  include Monad.T2 with type ('a, 'z) t := ('a, 'z) t
end
  
