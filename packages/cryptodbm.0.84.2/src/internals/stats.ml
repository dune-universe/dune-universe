
(* Statistics about keys and data. *)
type part = {
    (* Nb of elements seen. *)
    nb: int ;

    (* Sum of element lengths. *)
    sum: float ;

    (* Sum of squared lengths. *)
    sum2: float ;
  }

type t = {
    mutable keys: part ;
    mutable data: part ;
  }


(* To avoid division by zero, we assume a first element. *)
let init_part = {
  nb = 1 ;
  sum = 1.0 ;
  sum2 = 1.0 ;
}

let new_stats () =
  { keys = init_part ;
    data = init_part }

let put_part part s = 
  let len = float_of_int (String.length s) in
  { nb   = part.nb + 1 ;
    sum  = part.sum +. len ;
    sum2 = part.sum2 +. len *. len }

(* Returns the 'standard deviation' interval, that is, [mean - st deviation, mean + st deviation].
 * It is returned in the form (lower bound, interval length). 
 * The lower bound is guaranteed to be >= 0. *)
let get_interval part =
  let nb = float_of_int part.nb in
  let mean = part.sum /. nb in
  let dev  = sqrt ((part.sum2 /. nb) -. mean *. mean) in
  (max 0 (int_of_float (0.5 +. mean -. dev)), 
   int_of_float (0.5 +. 2.0 *. dev))

(* Choose an integer value in the standard deviation interval. *)
let choose (low, len) = low + Random.int len
  
let put stat key data =
  let key = Kinds.encodedkey2s key
  and data = Kinds.encodeddata2s data in
  
  stat.keys <- put_part stat.keys key ;
  stat.data <- put_part stat.data data ;
  ()

let insert stat coef add_fun =
  
  (* Choose the number of inserted bindings. *)
  let bound = (stat.keys.nb * coef) / 100 in

  (* If the bound is too small, we take at least 10 (as a bound). *)
  let nb = Random.int (max 10 bound) in

  let key_interval = get_interval stat.keys
  and data_interval = get_interval stat.data in

  for i = 1 to nb do
    let key_len = choose key_interval
    and data_len = choose data_interval in

    let key = Utils.random_string Utils.gen key_len
    and data = Utils.random_string Utils.gen data_len in
    add_fun ~key ~data
  done ;
  ()
