module M = struct
  let x = 1
  let (* M.x => *) x (* <= M.x *) = 2
end

let _ = M.x (* ? M.x *)
