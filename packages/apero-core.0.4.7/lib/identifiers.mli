open Ordered
module Id : sig

  module type S = sig
    type t
    include Stringable.S with type t := t
    include Comparable with type t := t
  end

  module Make(T : S) : S
end


module NumId : sig

  module type NumType = sig
    type t 
    include Id.S with type t := t
    val zero : t 
    val one : t 
    val add : t -> t -> t  
    val of_string_opt : string -> t option   
  end

  module type S = sig 
    type t 
    
    include NumType with type t := t

    val next_id : unit -> t
  end 

  module Make(T : NumType) : S

end
