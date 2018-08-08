module M = struct
  let bar = 342
end

module (* F => *) F (* <= F *) (S : sig val bar : int end) = struct
  include S
  let bar2 = S.bar
  let _ = bar2
  let _ = bar
end

module N = F (* ? F *)(M)
