module type (* S => *) S (* <= S *) = sig type (* S.t => *) t (* <= S.t *) end


module M : S (* ? S *)

module N : sig
  type t = C of M.t (* ? S.t *)
end

module O : sig type (* O.t => *) t (* <= O.t *) end

module P : sig
  type t = C of O.t (* ? O.t *)
end
  
