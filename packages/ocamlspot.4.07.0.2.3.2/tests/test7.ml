module M = struct
  module N = struct
    let (* M.N.x => *) x (* <= M.N.x *) = 1
  end
  let _ = N.x (* ? M.N.x *)
end

module O = struct
  module N = struct
    let (* O.N.y => *) y (* <= O.N.y *) = 1
  end
  let _ = N.y (* ? O.N.y *) 
end

module (* P => *) P (* <= P *) = struct
  module Q = struct
    let x = 1
    include Char
    let _ = uppercase
    module String = String
  end
end

