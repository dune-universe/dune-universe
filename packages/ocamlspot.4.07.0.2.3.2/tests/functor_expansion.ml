module F (S : sig val bar : int end) = struct
  include S
  let (* F.bar2 => *) bar2 (* <= F.bar2 *) = 
    S.bar 
  let _ = bar2 (* ? F.bar2 *)
end
