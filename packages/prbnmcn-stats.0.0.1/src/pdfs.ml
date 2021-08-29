(** Probability density functions. *)

let poisson ~lambda =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson"
  else
    let log_lambda = log lambda in
    fun ~k ->
      exp
      @@ ((float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k)

let pi = acos ~-.1.0

let gaussian ~mean ~std =
  if std <=. 0.0 then invalid_arg "Pdfs.gaussian" ;
  let normalizer = 1.0 /. (std *. (sqrt @@ (2.0 *. pi))) in
  fun x ->
    let delta = ((x -. mean) /. std) ** 2.0 in
    normalizer *. (exp @@ (~-.0.5 *. delta))

let exponential ~rate x = rate *. exp (~-.rate *. x)

let geometric ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.geometric" ;
  if k < 0 then invalid_arg "Pdfs.geometric" ;
  ((1. -. p) ** float_of_int k) *. p

let log_geometric ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.log_geometric" ;
  if k < 0 then invalid_arg "Pdfs.log_geometric" ;
  (log (1. -. p) *. float_of_int k) +. log p

let uniform { Stats_intf.min; max } (x : float) =
  let delta = max -. min in
  if delta >=. 0. then if x >=. min && x <=. max then 1. /. delta else 0.0
  else invalid_arg "uniform"
