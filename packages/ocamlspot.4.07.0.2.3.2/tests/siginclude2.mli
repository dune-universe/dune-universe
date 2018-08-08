module M : sig
  module type MT = sig type (* t => *) t (* <= t *) end
  include MT
  type s0 = t (* ? t *)
end

module N : sig
  type s = M.t (* ? t *)
end
