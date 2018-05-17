(* Definition of terms:

   A partition map is a way to represent a function, an association, a map.

   Parameters - These are the keys of the partition map; the set of things for
   which we want to keep track of some association. In our case the set are
   the intergers in [0,n) (n = parameter_size ).
*)

open Util
open Printf
open StdLabels

type parameters =
(* The number of elements in universe, the parameters over which we want to
   compute some relation. *)
  { domain_size : int

(* How many states we test in this example, or how many times we're going to
   merge. State in this example, is a map from parameters to some values,
   we are concerned with combining them. *)
  ; number_of_states  : int

(* The variaton in the states that we need to merge.

   Our state is an observation of some distribution over our parameters.
   This is crucial to why we might want to use partition maps, the total
   function should not have some solution ahead of time.

   Each state will have a finite number of unique value.We'll use a poisson
   distribution to determine how many. To keep things bounded, we'll assume
   that there will be 2.5 values on average.
*)
  ; average_number_of_values : float

(* For a given state, once we know the number of values, we have to assign them
   to our parameters, but we want them cluster together. The actual method of
   assigning might be dependent on the actual PM representation, so to keep
   things simple we will generate where the unique values start and stop. But
   the number intervals isn't the same as the number of values as some
   parameters later on might take previous values:

   [1, 10] -> 0, [11, 90] -> 1, [91, 100] -> 0

   So the number of start & stops will also be poisson, but greater.

*)
  ; average_number_of_intervals_incr : float
  }

let parameters_to_string
  { domain_size
  ; number_of_states
  ; average_number_of_values
  ; average_number_of_intervals_incr
  } =
  sprintf "Size of domain: %d\n\
           Number of states to merge: %d\n\
           Average number of values (per state): %f\n\
           Intervals incr: %f"
    domain_size number_of_states
    average_number_of_values average_number_of_intervals_incr

let parameters_to_encoded
  { domain_size
  ; number_of_states
  ; average_number_of_values
  ; average_number_of_intervals_incr
  } =
  sprintf "( %d, %d, %.1f, %.1f)"
    domain_size
    number_of_states
    average_number_of_values
    average_number_of_intervals_incr

let default_parameters =
  { domain_size = 1000
  ; number_of_states = 1000
  ; average_number_of_values = 1.5
  ; average_number_of_intervals_incr = 1.0
  }

let generate_parameters
    ?average_number_of_values
    ?average_number_of_intervals_incr
    ~domain_sizes ~number_of_states ()
  =
  let anv =
    match average_number_of_values with
    | None -> default_parameters.average_number_of_values
    | Some a -> a
  in
  let ani =
    match average_number_of_intervals_incr with
    | None -> default_parameters.average_number_of_intervals_incr
    | Some a -> a
  in
  List.map domain_sizes ~f:(fun ds ->
    List.map number_of_states ~f:(fun ns ->
      { domain_size = ds
      ; number_of_states = ns
      ; average_number_of_values = anv
      ; average_number_of_intervals_incr = ani
      }))
  |> List.concat

(* Draw from the poisson distribution.

   This is a HUGE assumption about your data. I chose Poisson because in a
   good sense it is the simplest distribution for the support (positive ints)
   that we use in the demo. *)
let poisson lambda =
  if lambda < 0. then
    invalid_argf "Negative lambda: %f" lambda
  else
    let p = exp (~-.lambda) in
    let rec loop u p s x =
      if u <= s then x
      else
        let nx = x + 1 in
        let np = p *. lambda /. (float nx) in
        let ns = s +. np in
        loop u np ns nx
    in
    fun () ->
      let u = Random.float 1. in
      loop u p p 0

let assign_starts_and_stops last_end n =
  let rec loop acc s n =
    if n = 1 then
      List.rev ((s, last_end) :: acc)
    else
      let e = last_end - s - n in
      let stop = s + Random.int e in
      loop ((s, stop) :: acc) (stop + 1) (n - 1)
  in
  loop [] 0 n

module type Calculation = sig
  val description : string
  type t
  val zero : t
  val op : t -> t -> t
  val op3 : t -> t -> t -> t

  (* How we initialize values. We take positive integer values from the Poisson
     distribution and map them to this data type. *)
  val of_int : int -> t
  val equal : t -> t -> bool
end

module Benchmarks (C : Calculation) = struct

  (*let random_assignment p =
    let number_of_values = poisson p.average_number_of_values in
    let average_number_of_intervals =
      p.average_number_of_values +. p.average_number_of_intervals_incr
    in
    let number_of_intervals = poisson average_number_of_intervals in
    let last_end = p.domain_size - 1 in
    let nov = 1 + number_of_values () in
    if nov = 1 then
      [(0, last_end), 0]
    else
      let noi = 1 + number_of_intervals () in
      let ss = assign_starts_and_stops last_end noi in
      let starting_value = Random.int nov in
      List.mapi ~f:(fun i ss ->
          let j = (i + starting_value) mod nov in
          ss, j) ss
    *)

  let random_assignment p =
    let number_of_values = poisson p.average_number_of_values in
    let average_number_of_intervals =
      p.average_number_of_values +. p.average_number_of_intervals_incr
    in
    let number_of_intervals = poisson average_number_of_intervals in
    let last_end = p.domain_size - 1 in
    let nov = 1 + number_of_values () in
    if nov = 1 then
      [(0, last_end), C.zero]
    else
      let noi = 1 + number_of_intervals () in
      let ss = assign_starts_and_stops last_end noi in
      let starting_value = Random.int nov in
      List.mapi ~f:(fun i ss ->
          let j = (i + starting_value) mod nov in
          ss, C.of_int j) ss

  (*
    How we assign to the intervals can be different, and will not be
    benchmarked....
    After we have these calculations we merge all of them into one final state!
  *)

  let states p =
    Array.init p.number_of_states
      ~f:(fun _ -> random_assignment p)

  (* Using int lists but mapping into an array. *)
  let time_list_merge p list_states =
    let acc = Array.make p.domain_size C.zero in
    fun () ->
      Array.iter list_states ~f:(fun lst ->
        List.iter lst ~f:(fun ((s, e), v) ->
            for i = s to e do
              acc.(i) <- C.op acc.(i) v
            done));
      `Array acc

  let time_list_merge3 p list_states =
    let acc = Array.make p.domain_size C.zero in
    let n = Array.length list_states in
    let rec loop2 l1 l2 = match l1, l2 with
      | [], []  -> ()
      | [],  _
      | _,  []  -> invalid_arg "One interval too empty!"
      | ((s1, e1), v1) :: t1
      , ((s2, e2), v2) :: t2 ->
          assert (s1 = s2);
          if e1 = e2 then begin
            for i = s1 to e1 do acc.(i) <- C.op3 acc.(i) v1 v2 done;
            loop2 t1 t2
          end else if e1 < e2 then begin
            for i = s1 to e1 do acc.(i) <- C.op3 acc.(i) v1 v2 done;
            loop2 (((e1+1,e2),v1)::t1) t2
          end else (* e1 > e2 *)
            for i = s1 to e2 do acc.(i) <- C.op3 acc.(i) v1 v2 done;
            loop2 t1 (((e2+1,e1),v2)::t2)
    in
    fun () ->
      for i = 0 to n - 2 do
        let ls1 = list_states.(i) in
        let ls2 = list_states.(i+1) in
        loop2 ls1 ls2
      done;
      `Array acc

  (* Using naive matrices. *)

  let states_as_arrays p states =
    Array.map states ~f:(fun lst ->
      let a = Array.make p.domain_size C.zero in
      List.iter lst ~f:(fun ((s, e), v) ->
          for i = s to e do a.(i) <- v done);
      a)

  let time_array_merge p array_states =
    let acc = Array.make p.domain_size C.zero in
    let last_end = p.domain_size - 1 in
    fun () ->
      Array.iter array_states ~f:(fun a ->
        for i = 0 to last_end do
          acc.(i) <- C.op acc.(i) a.(i)
        done);
      `Array acc

  let time_array_merge3 p array_states =
    let acc = Array.make p.domain_size C.zero in
    let last_end = p.domain_size - 1 in
    let n = Array.length array_states in
    fun () ->
      for i = 0 to n - 2 do
        let a = array_states.(i) in
        let b = array_states.(i + 1) in
        for i = 0 to last_end do
          acc.(i) <- C.op3 acc.(i) a.(i) b.(i)
        done;
      done;
      `Array acc

  (* Using association lists backed by bitvectors lists. *)

  (* An ugly hack to avoid redesigning the now deprecated bitvector methods. *)
  let size_ref = ref 0

  module Bv_sets = struct

    open Lib08

    type t = Bitvector.t
    let union = Bitvector.union
    let init () = Bitvector.create ~size:(!size_ref) false

    type inter_diff = { intersection : t; difference : t; same : bool; none : bool; }
    let inter_diff t1 t2 =
      let intersection, difference, same, none =
        Bitvector.inter_diff t1 t2 in
      { intersection; difference; same; none}

    let iter_set_indices t ~f =
      Bitvector.iter_true t f

    let complement = Bitvector.negate

    let of_interval (s, e) =
      let bv = init () in
      for i = s to e do Bitvector.set bv i done;
      bv

    let to_string t =
      Bitvector.print Format.std_formatter t;
      Format.flush_str_formatter ()

    end (* Bv_sets *)

  module Bv_assocs = Lib08.Assoc_list.Make(Bv_sets)

  let states_as_bv_assocs p states =
    size_ref := p.domain_size;
    Array.map states ~f:(fun lst ->
      List.map lst ~f:(fun (i, v) -> Bv_sets.of_interval i, v)
      |> Bv_assocs.of_list)

  let time_bv_assoc_merge bv_states =
    let init = Bv_assocs.init_everything C.zero in
    fun () ->
      `Bv (Array.fold_left bv_states ~init ~f:(fun s1 s2 ->
            Bv_assocs.map2 s1 s2 ~f:C.op))

  let time_bv_assoc_merge3 bv_states =
    let acc = ref (Bv_assocs.init_everything C.zero) in
    let n = Array.length bv_states in
    fun () ->
      for i = 0 to n - 2 do
        let b1 = bv_states.(i) in
        let b2 = bv_states.(i+1) in
        acc := Bv_assocs.map3 !acc b1 b2 ~f:C.op3
      done;
      `Bv (!acc)

  (* Using old partition maps. *)
  module PmaIp = Lib09.Partition_map.Ascending
  let states_as_pmas_ip states =
    Array.map states ~f:(PmaIp.of_ascending_interval_list C.equal)

  let time_pmas_ip_merge p pm_states =
    let size = p.domain_size in
    let starting_ascending_pm = PmaIp.init ~size C.zero in
    fun () ->
      `PmaIp (Array.fold_left pm_states ~init:starting_ascending_pm
          ~f:(fun a p -> PmaIp.merge C.equal a p C.op))

  let time_pmas_ip_merge3 p pm_states =
    let size = p.domain_size in
    let acc = ref (PmaIp.init ~size C.zero) in
    let n = Array.length pm_states in
    fun () ->
      for i = 0 to n - 2 do
        let p1 = pm_states.(i) in
        let p2 = pm_states.(i+1) in
        acc := PmaIp.merge3 C.equal !acc p1 p2 C.op3
      done;
      `PmaIp !acc

  (* Using partition maps. Pma = Ascending Partition Maps, the default kind.*)
  module Pma = Partition_map.Ascending

  let states_as_pmas states =
    Array.map states ~f:(Pma.of_ascending_interval_list ~eq:C.equal)

  let time_pmas_merge p pm_states =
    let size = p.domain_size in
    let starting_ascending_pm = Pma.init ~size C.zero in
    fun () ->
      `Pma (Array.fold_left pm_states ~init:starting_ascending_pm
          ~f:(fun a p -> Pma.merge ~eq:C.equal a p ~f:C.op))

  let time_pmas_merge3 p pm_states =
    let size = p.domain_size in
    let acc = ref (Pma.init ~size C.zero) in
    let n = Array.length pm_states in
    fun () ->
      for i = 0 to n - 2 do
        let p1 = pm_states.(i) in
        let p2 = pm_states.(i+1) in
        acc := Pma.merge3 ~eq:C.equal !acc p1 p2 ~f:C.op3
      done;
      `Pma !acc

  (* Reduction to array to check that we're computing the same thing. *)
  let as_array pr = function
    | `Array a  -> a
    | `Bv b     ->
        let a = Array.make pr.domain_size C.zero in
        Bv_assocs.iter_values b ~f:(fun i v -> a.(i) <- v);
        a
    | `Pma p    ->
        let a = Array.make pr.domain_size C.zero in
        Pma.fold_indices_and_values p ~init:()
          ~f:(fun () i v -> a.(i) <- v);
        a
    | `PmaIp p ->
        let a = Array.make pr.domain_size C.zero in
        PmaIp.fold_indices_and_values p ~init:()
          ~f:(fun () i v -> a.(i) <- v);
        a

  let name p s =
    sprintf "%s of %s %s" s C.description (parameters_to_encoded p)

  let which_to_tests p states = function
    | `NaiveList  ->
        name p "Naive lists into array"
        , time_list_merge p states
    | `NaiveArray ->
        let arrays = states_as_arrays p states in
        name p "Naive two array"
        , time_array_merge p arrays
    | `BvAssoc    ->
        let bvas   = states_as_bv_assocs p states in
        name p "Bitvector backed assocs"
        , time_bv_assoc_merge bvas
    | `PmaIp      ->
        let pmas   = states_as_pmas_ip states in
        name p "Paired Interval partition maps"
        , time_pmas_ip_merge p pmas
    | `Pma        ->
        let pmas   = states_as_pmas states in
        name p "Ascending partition maps"
        , time_pmas_merge p pmas

  let which_to_tests3 p states = function
    | `NaiveList  ->
        name p "Naive lists into array"
        , time_list_merge3 p states
    | `NaiveArray ->
        let arrays = states_as_arrays p states in
        name p "Naive two array"
        , time_array_merge3 p arrays
    | `BvAssoc    ->
        let bvas   = states_as_bv_assocs p states in
        name p "Bitvector backed assocs"
        , time_bv_assoc_merge3 bvas
    | `PmaIp      ->
        let pmas   = states_as_pmas_ip states in
        name p "Paired Interval partition maps"
        , time_pmas_ip_merge3 p pmas
    | `Pma        ->
        let pmas   = states_as_pmas states in
        name p "Ascending partition maps"
        , time_pmas_merge3 p pmas

  let generate_tests ?(two_or_three=`Two) which  p =
    let states = states p in
    let tests =
      match two_or_three with
      | `Two -> List.map which ~f:(which_to_tests p states)
      | `Three -> List.map which ~f:(which_to_tests3 p states)
    in
    let () =
      let results_as_arrays =
        List.map tests ~f:(fun (name, t) -> name, as_array p (t ()))
      in
      match results_as_arrays with
      | []     -> ()
      | h :: t ->
        ignore(List.fold_left t ~init:h
            ~f:(fun (pname, pt) (name, r) ->
                  if pt <> r then
                    invalid_argf "Results between %s and %s differ"
                      pname name
                  else
                    (name, r)))
    in
    tests

end (* Benchmarks *)

module IntBenchmarks =
  Benchmarks (struct
    let description = "Integer values"
    type t = int
    let zero = 0
    let op = ( + )
    let op3 x y z = x + y + z
    let of_int x = x
    let equal (x : int) y = x = y
  end)

module FloatBenchmarks =
  Benchmarks (struct
    let description = "Float values addition"
    type t = float
    let zero = 0.0
    let op = ( +. )
    let op3 x y z = x +. y +. z
    let of_int = float
    let equal (x : float) y = x = y
  end)

module FloatMBenchmarks =
  Benchmarks (struct
    let description = "Float values multiplication"
    type t = float
    let zero = 1.0
    let op = ( *. )
    let op3 x y z = x *. y *. z
    let of_int x = float (x + 1)  (* Avoid zero... otherwise PM's have a HUGE advantage. *)
    let equal (x : float) y = x = y
  end)

module IntVector = struct

  let description = "Integer vector"
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  let zero =
    { x = 0
    ; y = 0
    ; z = 0
    }
  let op t1 t2 =
    { x = t1.x + t2.x
    ; y = t1.y + t2.y
    ; z = t1.z + t2.z
    }
  let op3 t1 t2 t3 =
    { x = t1.x + t2.x + t3.x
    ; y = t1.y + t2.y + t3.y
    ; z = t1.z + t2.z + t3.z
    }

  let of_int t =
    let rec r = function
      | 0 -> { x = 1; y = 0; z = 0}
      | 1 -> { x = 0; y = 1; z = 0}
      | 2 -> { x = 0; y = 0; z = 1}
      | n -> op (r (n - 1)) (r (n - 2))
    in
    r t

  let equal t1 t2 =
    t1.x = t2.x && t1.y = t2.y && t1.z = t2.z

end (* IntVector *)

module IntVectorBenchmarks = Benchmarks (IntVector)

module FloatVector = struct

  let description = "Float vector"

  type t = float array

  let zero =
    [|0.;0.;0.;0.;0.;|]

  let dx = 1.e-6

  let is_nan x = x <> x

  let close_enough x y =
    if is_nan x then
      if is_nan y then
        true
      else
        false
    else if is_nan y then
      false
    else
      let d = x -. y in
      (abs_float d) < dx

(*
  let op t1 t2 =
    let a = Array.make 5 0. in
    let carry = ref 0. in
    for i = 0 to 4 do
      let j = float (1 lsl (5 - i)) in
      let s = !carry +. t1.(i) +. t2.(i) in
      if close_enough s j then begin
        a.(i) <- 0.;
        carry := 1.
      end else begin
        a.(i) <- s;
        carry := 0.
      end
    done;
    a

  let of_int t =
    let rec r = function
      | 0 -> [|1.;0.;0.;0.;0.|]
      | 1 -> [|0.;1.;0.;0.;0.|]
      | 2 -> [|0.;0.;1.;0.;0.|]
      | 3 -> [|0.;0.;0.;1.;0.|]
      | 4 -> [|0.;0.;0.;0.;1.|]
      | n -> op (r (n - 1)) (r (n - 2))
    in
    r t
   *)

  let op t1 t2 =
    Array.map2 t1 t2 ~f:(+.)

  let op3 t1 t2 t3 =
    Array.init (Array.length t1) ~f:(fun i -> t1.(i) +. t2.(i) +. t3.(i))

  let of_int t =
    let rec r = function
      | 0 -> [|1.;0.;0.;0.;0.|]
      | 1 -> [|0.;1.;0.;0.;0.|]
      | 2 -> [|0.;0.;1.;0.;0.|]
      | 3 -> [|0.;0.;0.;1.;0.|]
      | 4 -> [|0.;0.;0.;0.;1.|]
      | n -> r (n mod 5)
    in
    r t

  let equal t1 t2 =
    Array.for_all ~f:(fun x -> x) (Array.map2 ~f:close_enough t1 t2)

end (* FloatVector *)

module FloatVectorBenchmarks = Benchmarks (FloatVector)
