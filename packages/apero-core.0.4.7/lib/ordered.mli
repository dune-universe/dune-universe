
module type Comparable = sig 
  type t 
  val compare : t -> t -> int      
  val equal : t -> t -> bool
end

module Ordered : sig 

  module type S = sig 
    type t 
    include Comparable with type t := t

    module Infix : sig 
      val (=) : t -> t -> bool
      val (>) : t -> t -> bool
      val (>=) : t -> t -> bool
      val (<) : t -> t -> bool
      val (<=) : t -> t -> bool
      val (<>) : t -> t -> bool
    end
  end

  module Make (C : Comparable) : S with type t = C.t

end
