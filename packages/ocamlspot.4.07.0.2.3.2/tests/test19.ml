(* recursive *)

module rec M : sig val m : int -> int end = struct
  let (* M.m => *) m (* <= M.m *) x = N.n (* ? N.n *) x
end 

and N : sig val n : int -> int end = struct
  let (* N.n => *) n (* <= N.n *) x = M.m (* ? M.m *) x
end
