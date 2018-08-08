module M(A : sig type t end) = struct
  let _ = prerr_endline "M(A)"
  module type (* M(A).S => *) S   (* <= M(A).S *) = sig
    type t = A.t list
  end

end

module A = struct type t = int end

(*
module MA = M(A)

module N0 : MA.S = struct
  type t = int list
end
*)

module N1 : M(A).S (* ? M(A).S *) = struct
  type t = int list
end

(*
module N2 : M(struct type t = int end).S = struct
  type t = int list
end
*)

