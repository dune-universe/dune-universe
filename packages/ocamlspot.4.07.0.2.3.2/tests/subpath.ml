module F(M : sig end) = struct
  type (* t => *) t (* <= t *)
end

module (* N => *) N (* <= N *) = struct
end


module O = N

type fnt = F( N(* ? N *)  ).t (* ? t *)
