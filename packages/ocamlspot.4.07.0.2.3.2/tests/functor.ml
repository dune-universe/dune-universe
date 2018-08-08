module F (A : sig val x : int end) = struct
  let y = A.x
end

module M = struct let x = 1 end

module N = F(M)

let _ = N.y
