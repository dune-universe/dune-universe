(* To see ppx_monadic does not remove other let %xxx things by mistake *)

(* do *)
let _ = 
  do_;
  let %lwt x = 1 in
  return 1

(* pguard *)
let _ = match 1 with
  | 2 when let %lwt x = 1 in true -> 3

(* comprehension *)
let _ = [%comp x || let %lwt y = 1 in x <-- y ]
