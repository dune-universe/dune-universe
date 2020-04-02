module StateFunc : sig 
  module type S = sig        
    type s 
    type 'a m = s -> s * 'a
    val return : 'a -> s -> s * 'a    
    val bind : (s -> 'a * 'b) -> ('b -> 'a -> 'c) -> s -> 'c
    val read : s -> s * s
    val write : 'a -> s -> 'a * unit
    val run : (s -> 'a) -> s -> 'a
    val eval : (s -> 'a * 'b) -> s -> 'b
    val modify : (s -> 'a) -> s -> 'a * unit

    module Infix : sig
      val ( >>= ) : (s -> 'a * 'b) -> ('b -> 'a -> 'c) -> s -> 'c 
    end
  end 

  module Make (T : sig type s end) : S with type s = T.s
end


module State : sig 
  type ('s, 'a) m = 's -> 's * 'a
  val return : 'a -> 's -> 's * 'a    
  val bind : ('s -> 'a * 'b) -> ('b -> 'a -> 'c) -> 's -> 'c
  val read : 's -> 's * 's
  val write : 'a -> 's -> 'a * unit
  val run : ('s -> 'a) -> 's -> 'a
  val eval : ('s -> 'a * 'b) -> 's -> 'b
  val modify : ('s -> 'a) -> 's -> 'a * unit

  module Infix : sig 
    val ( >>= ) : ('s -> 'a * 'b) -> ('b -> 'a -> 'c) -> 's -> 'c 
  end
end 
