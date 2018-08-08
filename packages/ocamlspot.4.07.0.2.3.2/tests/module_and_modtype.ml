module O = struct
  module M = struct end
  module type (* O.M => *) M (* <= O.M *) = sig end 
end

module N : O.M (* ? O.M *) = struct end

