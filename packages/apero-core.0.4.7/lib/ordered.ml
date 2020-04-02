module type Comparable = sig 
  type t 
  val compare : t -> t -> int      
  val equal : t -> t -> bool
end

module Ordered = struct 

  module type S = sig   
    include Comparable 

    module Infix : sig 
      val (=) : t -> t -> bool
      val (>) : t -> t -> bool
      val (>=) : t -> t -> bool
      val (<) : t -> t -> bool
      val (<=) : t -> t -> bool
      val (<>) : t -> t -> bool
    end
  end

  module Make (C : Comparable) = struct
    include C
    let equal a b = (C.compare a b) = 0

    module Infix = struct 
      let (=) = equal
      let (>) a b = (C.compare a b) > 0
      let (>=) a b = (C.compare a b) >= 0
      let (<) a b = (C.compare a b) < 0
      let (<=) a b = (C.compare a b) <= 0
      let (<>) a b = (C.compare a b) <> 0
    end
  end
end
