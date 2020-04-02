open Ordered
module Id = struct

  module type S = sig
    type t
    include Stringable.S with type t := t
    include Comparable with type t := t
  end

  module Make(T: S) = struct
    include T
  end
end


module NumId = struct

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

  module Make(T : NumType) = struct 
    include T

    type state = {mutable count : T.t }

    let state = { count = T.zero }  
    
    let next_id () =
      let r = state.count in  
      (** roll-over to negatives and eventually back to zero*)
      state.count <- T.add state.count T.one ; r    
  end

end
