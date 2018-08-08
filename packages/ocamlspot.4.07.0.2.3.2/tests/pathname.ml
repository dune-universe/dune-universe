module M = struct
  module N = struct
    module O = struct
      module P = struct
	let q = 1
      end
    end
  end
end

module F( M : sig end ) = struct
  module N = struct
    let x = 1
  end
end

let _ = M.N.O.P.q
let _ = (*comment*)M.(*comment*)N.O(*comment*).P.q

let _ = let module FM = F(M) in FM.N.x
