module M = struct
  let (* M.bar => *) bar (* <= M.bar *) = 42
end

module F(S : sig val bar : int end) = struct
  include S
end

module N = F(M)

let _ = N.bar (* ? M.bar *) 
