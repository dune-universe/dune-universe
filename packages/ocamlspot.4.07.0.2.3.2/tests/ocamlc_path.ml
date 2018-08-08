(* See camlp4_path.ml *)
module X = struct
  let (* x => *) x (* <= x *) = 1
end

let _ = X.(* ? x *)x (* ? x *)
