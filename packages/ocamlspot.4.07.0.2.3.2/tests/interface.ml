module M : sig 
  exception E
  val x : int
end = struct
  exception E
  let x = 1
  let _ = x
end

module N = struct
  let y = 1
end

let _ = M.x
let _ = N.y

module O : sig
  module P : sig
    val z : int
  end 
end = struct
  module P = struct
    let z = 1
  end
end

let _ = O.P.z
