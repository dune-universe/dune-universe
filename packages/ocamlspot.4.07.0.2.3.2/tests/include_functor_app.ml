module (* F => *)F(* <= F *)(A : sig end) = struct
  let (* x => *) x (* <= x *) = 1
end


module M = struct
  include F (* ? F *) (struct end)
  let _ = x (* ? x *)
end

let _ = M.x (* ? x *)

