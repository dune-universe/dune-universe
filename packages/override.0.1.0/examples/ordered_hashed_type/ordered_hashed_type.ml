module type S = sig
  module%import Hashtbl : sig
    module type%import HashedType = sig
      [%%symbols]
    end
  end

  module%import Map : sig
    module type%import OrderedType = sig
      type t [@@remove]
      [%%symbols]
    end
  end
end
    
