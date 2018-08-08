module type (* SX => *) SX (* <= SX *) = sig
  type (* SX.t => *) t (* <= SX.t *) = int 
end

module type S = sig
  module X : SX (* ? SX *)

  type t = X.t (* ? SX.t *)
end
  
