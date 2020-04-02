open Ordered

module KeyValueF = struct  
  
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
    (V : Comparable) = struct 

    module Key = Ordered.Make(K)
    module Value = Ordered.Make(V)

    module Map = Map.Make(Key)    

    module C = struct 
      type t = Key.t * Value.t
      let compare (k1,v1) (k2,v2) = match (Key.compare k1 k2, Value.compare v1 v2) with 
      | (0, 0) -> 0
      | (a, _) -> a 
      let equal (k1,v1) (k2,v2) = Key.equal k1 k2 && Value.equal v1 v2
    end
    
    include Ordered.Make (C) 

    let make k v = (k, v)
    let key (k, _) = k
    let value (_, v) = v    
    
  end
end
