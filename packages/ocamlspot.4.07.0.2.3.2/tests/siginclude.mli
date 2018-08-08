module type T = sig type (* t => *) t (* <= t *) end

include T

type s = t (* ? t *)
