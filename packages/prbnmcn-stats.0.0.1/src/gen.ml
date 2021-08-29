type parameters = { nsamples : int; state : Random.State.t }

module M :
  Stats_intf.Sampling_monad
    with type 'a res =
          parameters ->
          (module Basic_intf.Std with type t = 'a) ->
          ('a, float) Stats_intf.fin_mes = struct
  type 'a t = Random.State.t -> 'a

  type 'a res =
    parameters ->
    (module Basic_intf.Std with type t = 'a) ->
    ('a, float) Stats_intf.fin_mes

  let bind m f state =
    let x = m state in
    f x state

  let map m f state =
    let x = m state in
    f x

  let return x _state = x

  let run :
      type a.
      a t ->
      parameters ->
      (module Basic_intf.Std with type t = a) ->
      (a, float) Stats_intf.fin_mes =
   fun sampler { nsamples; state } (module O) ->
    let module F = Basic_impl.Free_module.Float_valued.Make_with_map (O) in
    let (`Empirical empirical) =
      Emp.to_raw_data @@ Emp.of_generative ~nsamples sampler state
    in
    let map =
      Array.fold_left
        (fun map elt ->
          let map =
            match F.Map.find_opt elt map with
            | None -> F.Map.add elt 1.0 map
            | Some count' -> F.Map.add elt (1.0 +. count') map
          in
          map)
        F.Map.empty
        empirical
    in
    let bindings = F.Map.bindings map in
    Fin.Float.measure (module F) bindings

  module Infix = struct
    let ( >>= ) = bind

    let ( >|= ) = map

    let ( let* ) = bind

    let return = return
  end
end

include M

let float bound state = Random.State.float state bound

let int bound state = Random.State.int state bound

let bool = Random.State.bool

let range { Stats_intf.min; max } state =
  if max -. min >=. 0. then min +. Random.State.float state (max -. min)
  else invalid_arg "uniform_in_interval"

let _bernouilli h state =
  let x = Random.State.float state 1.0 in
  x <. h

let bernouilli h state =
  if h <. 0.0 || h >. 1.0 then invalid_arg "bernouilli" ;
  _bernouilli h state

let geometric p state =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "geometric" ;
  let failures = ref 0 in
  while not (_bernouilli p state) do
    incr failures
  done ;
  !failures

let uniform (elts : 'a array) =
  let len = Array.length elts in
  if len = 0 then invalid_arg "uniform" ;
  fun state ->
    let i = int len state in
    elts.(i)

let subsample ~n sampler : 'a t =
 fun rng_state ->
  let counter = ref 0 in
  let rec loop rng_state =
    let res = sampler rng_state in
    incr counter ;
    if Int.equal (!counter mod n) 0 then res else loop rng_state
  in
  loop rng_state

let of_empirical : 'a Stats_intf.emp -> 'a t =
 fun data rng_state ->
  let len = Array.length data in
  if len = 0 then invalid_arg "of_empirical" ;
  let i = Random.State.int rng_state len in
  data.(i)

module Float = struct
  let exponential ~rate : float t =
   fun rng_state ->
    let u = Random.State.float rng_state 1.0 in
    ~-.(log u) /. rate

  let box_muller : mean:float -> std:float -> (float * float) t =
    let rec reject_loop rng_state =
      let u = Random.State.float rng_state 2.0 -. 1.0 in
      let v = Random.State.float rng_state 2.0 -. 1.0 in
      let s = (u *. u) +. (v *. v) in
      if s =. 0.0 || s >=. 1.0 then reject_loop rng_state
      else
        let weight = sqrt (-2. *. log s /. s) in
        let variate1 = u *. weight in
        let variate2 = v *. weight in
        (variate1, variate2)
    in
    fun ~mean ~std rng_state ->
      let (v1, v2) = reject_loop rng_state in
      (mean +. (std *. v1), mean +. (std *. v2))

  type gaussgen_state = Fresh | Last of float

  let gaussian ~mean ~std : float t =
    let state = ref Fresh in
    let gen = box_muller ~mean ~std in
    fun rng_state ->
      match !state with
      | Fresh ->
          let (x1, x2) = gen rng_state in
          state := Last x2 ;
          x1
      | Last x ->
          state := Fresh ;
          x

  module Alias_f =
    Alias.Make (Basic_impl.Reals.Float) (Basic_impl.Reals.Float) (M)
      (struct
        type 'a t = 'a M.t

        let mass bound state = Random.State.float state bound

        let int bound state = Random.State.int state bound
      end)

  let categorical list =
    let s = Alias_f.create list in
    fun state -> Alias_f.sampler s state
end

include Float

let of_fin_mes : type a. (a, float) Stats_intf.fin_mes -> a t =
  fun (type a)
      (Stats_intf.M
        (module P : Stats_intf.Fin_fun with type t = a and type r = float)) ->
   let bindings = P.V.fold (fun x w acc -> (x, w) :: acc) P.weightmap [] in
   categorical bindings

module Rational = struct
  module Alias_q =
    Alias.Make (Basic_impl.Reals.Rational) (Basic_impl.Reals.Rational) (M)
      (struct
        type 'a t = 'a M.t

        let mass bound state =
          Basic_impl.Reals.Rational.(bound * lebesgue state)

        let int bound state = Random.State.int state bound
      end)

  let categorical list =
    let s = Alias_q.create list in
    fun state -> Alias_q.sampler s state
end
