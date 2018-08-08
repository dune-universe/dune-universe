module M = struct
  type t = (* M.t => *) Foo (* <= M.t *) of int 
end

let _ = M.Foo (* ? M.t *) 1
