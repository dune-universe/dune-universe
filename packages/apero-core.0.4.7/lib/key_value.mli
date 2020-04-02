open Ordered

module KeyValueF : sig 
  
  module type S = sig 
 
    module Key : Ordered.S      
    module Value : Ordered.S      
    
    module Map : (module type of Map.Make(Key))
    
    include Ordered.S with type t = Key.t * Value.t    

    val make : Key.t -> Value.t -> t     
    val key : t -> Key.t 
    val value : t -> Value.t        
  end


  module Make 
    (K : Comparable)
    (V : Comparable) : S with type Key.t = K.t and type Value.t = V.t 
end