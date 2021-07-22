type rng = { mutable state : int64 }

type bounded_rng = {
  mutable state : int64;
  bound : int64;
  threshold : int64;
}

let modulus = 0x7FFF_FFFFL

let check_bound bound = assert (Int64.unsigned_compare bound modulus < 0)

let init_state seed =
  let seed = Int64.of_int seed in
  if seed = 0L then 1L else Int64.(logand seed modulus)

let calc_threshold bound = Int64.(unsigned_rem modulus bound)

let create_rng seed : rng = { state = init_state seed }

let create_bounded_rng ~bound seed : bounded_rng =
  let bound = Int64.of_int bound in
  check_bound bound;
  let threshold = calc_threshold bound in
  { state = init_state seed; bound; threshold }

let hash' (x : int64) : int64 =
  let x = Int64.(mul x 48271L) in
  Int64.(unsigned_rem x modulus)

let hash_int (x : int) : int = Int64.to_int @@ hash' (Int64.of_int x)

let gen' (rng : rng) (bound : int64) : int64 =
  let rec aux (rng : rng) bound threshold =
    let x = hash' rng.state in
    rng.state <- x;
    if Int64.unsigned_compare x threshold >= 0 then Int64.unsigned_rem x bound
    else aux rng bound threshold
  in
  check_bound bound;
  aux rng bound (calc_threshold bound)

let gen_bounded' (rng : bounded_rng) : int64 =
  let rec aux (rng : bounded_rng) =
    let x = hash' rng.state in
    rng.state <- x;
    if Int64.unsigned_compare x rng.threshold >= 0 then
      Int64.unsigned_rem x rng.bound
    else aux rng
  in
  aux rng

let global =
  Random.self_init ();
  let seed = Random.int 0x0FFF_FFFF in
  create_rng seed

let gen_int (rng : rng) (bound : int) : int =
  let r = Int64.to_int @@ gen' rng (Int64.of_int bound) in
  r

let gen_int_global bound = gen_int global bound

let gen_int_bounded (rng : bounded_rng) : int =
  let r = Int64.to_int @@ gen_bounded' rng in
  r
