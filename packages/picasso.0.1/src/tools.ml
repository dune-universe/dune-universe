let foi = float_of_int
let iof = int_of_float

(* helper : project from a value n from [a;b] to [c;d] *)
let projection (a,b) (c,d) n =
  let perc (x,y) r = x +. (r *. (y-.x))
  and to_perc (x,y) r =
    if x < 0. then (r-.x) /. (y-.x)
    else (r-.x) /. (y-.x)
  in
  if b = a then c else perc (c,d) (to_perc (a,b) n)
