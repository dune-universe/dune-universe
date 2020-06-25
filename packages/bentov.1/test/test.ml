(* In this test, we measure the error between the true quantiles of
   data, and the quantiles as computed through the [Bentov] histogram
   approximation module. The data is drawn from a mixture of two
   Gaussians, N(2,1) and N(5,1), where the mixing coefficient is 1/2.
   We compute the approximate quantiles in two ways: In the frist, we
   simply add each sample into a [Bentov.histogram]. We call this
   histogram [mixed]. In the second, we add a datum to one of two
   [Bentov.histogram]'s, one associated with each of the Guassians. We
   then merge these two histograms using [Bentov.merge]. We call the
   result of merging the two sub-histograms [merged]. Finally, we
   compute and print the mean-square-error between the true quantiles
   and [mixed], and the true quantiles and [merged]. *)

(* [quantiles list num_intervals] returns the quantiles (true, not
   approximated) of list [list] at [num_intervals + 1] points, including
   the minimum and maximum values (which are the first and last values of
   the result, resp. *)
let quantiles =
  let rec loop i j span accu = function
    | x0 :: x1 :: rest ->
      let s = (float j) *. span in
      if i <= s && s < i +. 1. then
        let d = s -. i in
        let x_d = (x1 -. x0) *. d +. x0 in
        loop i (j + 1) span ((j, x_d) :: accu) (x0 :: x1 :: rest)
      else
        loop (i +. 1.) j span accu (x1 :: rest)

    | [x] ->
      let x_d = j, x in
      List.rev (x_d :: accu)

    | [] -> assert false
  in
  fun x m ->
    let x_sorted = List.sort Stdlib.compare x in
    let n = List.length x_sorted in
    let span = (float n) /. (float m) in
    loop 0. 0 span [] x_sorted


open Seq

(** [up_to n seq] returns a sequence that echos at most [n] values of
    sequence [seq] *)
let up_to =
  let rec loop i n seq () =
    if i = n then
      Nil
    else
      match seq () with
      | Nil -> Nil
      | Cons (v, seq) ->
        Cons (v, loop (i + 1) n seq)
  in
  fun n seq ->
    if n < 0 then
      raise (Invalid_argument "up_to")
    else
      loop 0 n seq

module IntMap = Map.Make(Int)

let map_of_assoc assoc =
  List.fold_left (
    fun k_to_v (k, v) ->
      IntMap.add k v k_to_v
  ) IntMap.empty assoc

let _ =
  (* the number of data to draw *)
  let n = 100_000 in

  (* the size of the approximate histograms *)
  let q = 20 in

  let rec gaussian_mixture normal_a normal_b () =
    if Random.bool () then
      match normal_a () with
      | Cons (x, normal_a) ->
        Cons ((`A, x), gaussian_mixture normal_a normal_b)
      | Nil -> Nil
    else
      match normal_b () with
      | Cons (x, normal_b) ->
        Cons ((`B, x), gaussian_mixture normal_a normal_b)
      | Nil -> Nil
  in

  let n1 = Gaussian.seq ~mu:2.0 ~sigma:1.0 in (* N(2,1) *)
  let n2 = Gaussian.seq ~mu:5.0 ~sigma:1.0 in (* N(5,1) *)

  let gmm = up_to n (gaussian_mixture n1 n2) in

  let open Bentov in

  let normal_a_h, normal_b_h, mixed_h, data = Seq.fold_left (
    fun (normal_a_h, normal_b_h, mixed_h, data) draw ->
      match draw with
      | `A, x ->
        let normal_a_h = Bentov.add x normal_a_h in
        let mixed_h = Bentov.add x mixed_h in
        normal_a_h, normal_b_h, mixed_h, x :: data

      | `B, x ->
        let normal_b_h = Bentov.add x normal_b_h in
        let mixed_h = Bentov.add x mixed_h in
        normal_a_h, normal_b_h, mixed_h, x :: data
  ) (
    let normal_a_h = create q in
    let normal_b_h = create q in
    let mixed_h = create q in
    normal_a_h, normal_b_h, mixed_h, []
  ) gmm in

  (* merge the two sub-histograms *)
  let merged_h = merge [normal_a_h; normal_b_h] q in

  assert (total_count mixed_h = n );
  assert (total_count merged_h = n );

  (* measure the error between the true quantiles and approximations
     on a grid half the size of our approximate histograms *)
  let num_intervals = q/2 in

  let error i actual mixed merged =
    match IntMap.find_opt i actual with
    | None -> None, None
    | Some actual ->
      (match IntMap.find_opt i mixed with
       | Some mixed -> Some (actual -. mixed)
       | None -> None
      ),
      (match IntMap.find_opt i merged with
       | Some merged -> Some (actual -. merged)
       | None -> None
      )
  in

  (* compute sum of squared-errors *)
  let rec stats i actual mixed merged mixed_stats merged_stats =
    if i < num_intervals then
      let mixed_err, merged_err = error i actual mixed merged in
      let mixed_stats =
        match mixed_err with
        | Some err ->
          let sum_se_mixed, n_mixed = mixed_stats in
          sum_se_mixed +. err *. err, n_mixed + 1
        | None -> mixed_stats
      in
      let merged_stats =
        match merged_err with
        | Some err ->
          let sum_se_merged, n_merged = merged_stats in
          sum_se_merged +. err *. err, n_merged + 1
        | None -> merged_stats
      in
      stats (i+1) actual mixed merged mixed_stats merged_stats
    else
      mixed_stats, merged_stats
  in

  let mixed_q  = map_of_assoc (uniform mixed_h num_intervals) in
  let merged_q = map_of_assoc (uniform merged_h num_intervals) in
  let actual_q = map_of_assoc (quantiles data num_intervals) in

  let (sum_se_mixed, n_mixed), (sum_se_merged, n_merged) =
    stats 0 actual_q mixed_q merged_q (0., 0) (0., 0) in

  let err_mixed = sqrt ((sum_se_mixed) /. (float n_mixed)) in
  let err_merged = sqrt ((sum_se_merged) /. (float n_merged)) in
  Printf.printf "err_mixed=%e (n=%d)\nerr_merged=%e (n=%d)\n"
    err_mixed n_mixed err_merged n_merged;
  assert (err_mixed <= 5e2 && err_merged <= 5e2)

