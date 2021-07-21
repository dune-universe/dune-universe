open Core_kernel
open CFStream

type count_matrix = int array array
type background = float array
type t = float array array

let int_of_char = function
  | 'a' | 'A' -> 0
  | 'c' | 'C' -> 1
  | 'g' | 'G' -> 2
  | 't' | 'T' -> 3
  | _ -> 4

let flat_background () = Array.create ~len:4 0.25

let background_of_sequence seq pc =
  let counts = Caml.Array.make 4 0
  and n = ref 0 in
  for i = 0 to String.length seq - 1 do
    let k = int_of_char seq.[i] in
    if k < 4 then (
      counts.(k) <- counts.(k) + 1 ;
      incr n
    )
  done ;
  Array.map ~f:(fun c -> (float c +. pc) /. (float !n +. 4. *. pc)) counts

let reverse_complement a =
  let open Array in
  let n = length a in
  init n ~f:(fun i ->
      let r = copy a.(n - 1 - i) in
      swap r 0 3 ;
      swap r 1 2 ;
      r
    )

let length = Array.length

let make mat bg =
  let open Array in
  map mat ~f:(fun p ->
      let n = fold ~f:( + ) ~init:0 p in
      let r =
        mapi p ~f:(fun i x ->
            log ((float x +. bg.(i)) /. float n /. bg.(i))
          )
      in
      let n_case =
        Stream.Infix.(0 --^ (Array.length p))
        |> Stream.fold ~f:(fun accu i -> accu +. bg.(i) *. r.(i)) ~init:0. in
      append r [| n_case |])

let random_profile () =
  let v = Array.init 4 ~f:(fun _ -> Random.float 1.) in
  let s = Array.fold v ~f:( +. ) ~init:0. in
  Array.map v ~f:(fun x -> x /. s)

let random_background = random_profile
let random ~len background =
  let counts = Array.init len ~f:(fun _ ->
      Array.map (random_profile ()) ~f:(fun f -> Float.to_int (100. *. f))
    )
  in
  make counts background

let tandem ?(orientation = `direct) ~spacer mat1 mat2 bg =
  Array.concat [
    (match orientation with
     | `everted -> reverse_complement
     | `inverted | `direct -> ident) (make mat1 bg) ;
    Array.init spacer ~f:(fun _ -> Caml.Array.make 5 0.) ;
    (match orientation with
     | `inverted -> reverse_complement
     | `everted | `direct -> ident) (make mat2 bg)
  ]

let gen_scan f init mat seq tol =
  let r = ref init
  and n = String.length seq
  and m = Array.length mat in
  let seq = Array.init n ~f:(fun i -> int_of_char seq.[i]) in
  for i = n - m downto 0 do
    let score = ref 0. in
    for j = 0 to m - 1 do
      score := !score +. Array.(unsafe_get (unsafe_get mat j) (unsafe_get seq (i + j)))
    done ;
    if Float.(!score > tol)
    then r := f i !score !r
  done ;
  !r

let array_max (t : float array) =
  let best = ref Float.neg_infinity in
  for i = 0 to Array.length t - 1 do
    let t_i = t.(i) in
    if Float.(t_i > !best) then best := t_i
  done ;
  !best

let bs pwm =
  let n = Array.length pwm in
  let r = Array.create ~len:n (array_max pwm.(n - 1)) in
  for i = n - 2 downto 0 do
    r.(i) <- r.(i + 1) +. array_max pwm.(i)
  done ;
  r

let matrix_permutation mat =
  let max = Array.map mat ~f:array_max in
  Array.mapi mat ~f:(fun i col -> i, col)
  |> Array.sorted_copy ~compare:(fun (i,_) (j,_) -> Float.compare max.(j) max.(i))
  |> Array.map ~f:fst

exception Skip
let opt_scan f init mat seq tol =
  let r = ref init
  and n = String.length seq
  and m = Array.length mat in
  let seq = Array.init n ~f:(fun i -> int_of_char seq.[i]) in
  let sigma = matrix_permutation mat in
  let perm_mat = Array.map sigma ~f:(fun i -> mat.(i)) in
  let bs = bs perm_mat in
  for i = n - m downto 0 do
    try
      let score = ref 0. in
      for j = 0 to m - 1 do
        if Float.(!score +. bs.(j) < tol) then raise Skip ;
        score := !score +. Array.(unsafe_get (unsafe_get perm_mat j) (unsafe_get seq (i + sigma.(j))))
      done ;
      if Float.(!score > tol)
      then r := f i !score !r
    with Skip -> ()
  done ;
  !r

let scan = gen_scan (fun pos score l -> (pos, score) :: l) []
let opt_scan = opt_scan (fun pos score l -> (pos, score) :: l) []

let best_hit mat seq =
  let (pos, _) as r =
    gen_scan (fun p1 s1 ((_, s2) as r2) -> if Float.(s1 > s2) then (p1, s1) else r2) (-1, Float.neg_infinity) mat seq Float.neg_infinity
  in
  if pos < 0 then raise (Invalid_argument "Pwm.best_hit: sequence shorter than the matrix")
  else r

external stub_fast_scan : t -> string -> float -> (int * float) list = "biotk_pwm_scan"
external stub_opt_fast_scan : t -> string -> float -> (int * float) list = "biotk_opt_pwm_scan"

let fast_scan mat seq tol =
  stub_fast_scan mat seq tol

let opt_fast_scan mat seq tol =
  stub_opt_fast_scan mat seq tol

let check x y =
  List.(length x = length y)
  &&
  List.zip_exn x y
  |> List.for_all ~f:(fun ((i, x_i), (j, y_j)) ->
      i = j && Float.(abs (x_i - y_j) < 1e-6)
    )

let test f g =
  let s = "acgatcgatcgatgcatgctagctagctagctagctagctagcatcgatgcatgct" in
  let bg = random_background () in
  let pwm = random ~len:10 bg in
  let theta = Random.float 10. in
  check (f pwm s theta) (g pwm s theta)

let repeat_test f g =
  List.init 10 ~f:(fun _ -> test f g)
  |> List.for_all ~f:Fn.id

let%test "fast_scan = scan" =
  repeat_test scan fast_scan

let%test "opt_scan = scan" = repeat_test scan opt_scan

let%test "opt_scan = opt_fast_scan" = repeat_test opt_scan opt_fast_scan
