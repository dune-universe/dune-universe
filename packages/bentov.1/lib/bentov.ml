type bin = {
  center : float;
  count : int
}

type histogram = {
  max_bins : int;
  num_bins : int;
  bins : bin list;
  range : (float * float) option; (* (min * max) *)
  total_count : int;
}

let max_bins h =
  h.max_bins

let num_bins h =
  h.num_bins

let bins h =
  h.bins

let range h =
  h.range

let total_count h =
  h.total_count

(* not tail rec! *)
let rec insert value = function
  | [] -> [{ center = value ; count = 1 }], true
  | h :: t ->
    if value < h.center then
      { center = value ; count = 1 } :: h :: t, true
    else if value = h.center then
      { h with count = h.count + 1 } :: t, false
    else
      let t, num_bins_is_incr = insert value t in
      h :: t, num_bins_is_incr

let rec min_diff_index i index min_diff = function
  | a :: b :: t ->
    let diff = b.center -. a.center in
    assert ( diff > 0. );
    if diff < min_diff then
      min_diff_index (i+1) i diff (b :: t)
    else
      (* no change *)
      min_diff_index (i+1) index min_diff (b :: t)

  | [ _ ] -> index
  | [] -> assert false

let min_diff_index = function
  | a :: b :: t ->
    let diff = b.center -. a.center in
    assert ( diff > 0. );
    min_diff_index 1 0 diff (b :: t)

  | [ _ ]
  | [] -> assert false

let merge_bins lo hi =
  assert (lo.center < hi.center);
  let sum_count = lo.count + hi.count in
  let center =
    (* average of centers, weighted by their height *)
    (lo.center *. (float lo.count) +. hi.center *. (float hi.count)) /.
    (float sum_count)
  in
  { center; count = sum_count }

(* not tail rec! *)
let merge_bins_at_index =
  let rec loop i index = function
    | a :: b :: t ->
      if i = index then
        let bin = merge_bins a b in
        bin :: t
      else
        a :: (loop (i + 1) index (b :: t))

    | [ _ ]
    | [] -> assert false
  in
  fun index bins ->
    loop 0 index bins

let create max_bins =
  if max_bins < 2 then
    raise (Invalid_argument (Printf.sprintf "max_bins: %d" max_bins))
  else {
    max_bins;
    num_bins = 0;
    bins = [];
    total_count = 0;
    range = None
  }

let add value histogram =
  let range =
    match histogram.range with
      | Some (mn, mx) -> Some (min value mn, max value mx)
      | None -> Some (value, value)
  in
  let total_count = histogram.total_count + 1 in
  let bins, is_augmented = insert value histogram.bins in
  if histogram.num_bins = histogram.max_bins then
    if is_augmented then
      (* merge bins, so as to keep their number at [max_bins] *)
      let index = min_diff_index bins in
      let bins = merge_bins_at_index index bins in
      { histogram with bins; range; total_count }
    else
      { histogram with bins; range; total_count }
  else
    if is_augmented then
      { histogram with bins; range; total_count;
                       num_bins = histogram.num_bins + 1; }
    else
      { histogram with bins; range; total_count }

(* merge two sorted bin lists; not tail rec! *)
let rec binary_merge a b =
  match a, b with
    | a_h :: a_t, b_h :: b_t ->
      if a_h.center < b_h.center then
        a_h :: (binary_merge a_t b)
      else if a_h.center > b_h.center then
        b_h :: (binary_merge a b_t)
      else
        (* a_h.center = b_h.center: merge the two cells into one *)
        let merged = { a_h with count = a_h.count + b_h.count } in
        merged :: (binary_merge a_t b_t)

    | [], _ :: _ -> b
    | _ :: _, [] -> a
    | [], [] -> []

let rec k_ary_merge_half accu = function
  | a :: b :: t ->
    let ab = binary_merge a b in
    k_ary_merge_half  (ab :: accu) t

  | [a] -> (a :: accu)
  | [] -> accu

let rec k_ary_merge t =
  match k_ary_merge_half [] t with
    | [a] -> a
    | t -> k_ary_merge t


let rec reduce bins ~num_bins ~max_bins =
  if num_bins > max_bins then
    let index = min_diff_index bins in
    let bins = merge_bins_at_index index bins in
    reduce bins ~num_bins:(num_bins - 1) ~max_bins
  else
    bins



let merge h_list max_bins =
  let bins, _, total_count, range = List.fold_left
      (fun (t_bins, t_num_bins, t_total_count, t_range)
        { bins; num_bins; total_count; range; _} ->
        let t_range =
          match t_range, range with
            | Some (t_mn, t_mx), Some (mn, mx) ->
              Some ((min t_mn mn), (max t_mx mx))
            | None, Some _ -> range
            | Some _, None -> t_range
            | None, None -> None
        in
        bins :: t_bins,
        t_num_bins + num_bins,
        t_total_count + total_count,
        t_range
      ) ([], 0, 0, None) h_list in

  (* even if [num_bins <= output_max_bins], we have to apply
     [k_ary_merge] to combine indentical bin centers *)
  let merged_bins = k_ary_merge bins in
  let num_bins = List.length merged_bins in
  let bins = reduce merged_bins ~num_bins ~max_bins in
  let num_bins = List.length bins in
  { bins;
    num_bins;
    max_bins;
    total_count;
    range }

(* add a value with a count; equivalent to calling [add value hist]
   [count] times *)
let addc value count hist =
  let singleton = {
    bins = [{ center = value ; count }];
    total_count = count;
    range = Some (value, value);
    num_bins = 1;
    max_bins = 1; (* benign *)
  } in
  merge [hist; singleton] hist.max_bins

let pos_quadratic_root ~a ~b ~c =
  if a = 0.0 then
    -.c /. b
  else
    let discriminant = b *. b -. 4. *. a *. c in
    ((sqrt discriminant) -. b) /. (2. *. a)

exception Empty

let sum =
  let rec find_i b i sum = function
    | ({ center = p_i; count = m_i } as bin_i) ::
        ({ center = p_i1; _ } as bin_i1) :: t ->
      if p_i <= b && b < p_i1 then
        bin_i, bin_i1, sum
      else
        find_i b (i+1) (sum +. (float m_i)) (bin_i1 :: t)

    | _ -> raise Not_found
  in

  fun histogram b ->
    let {center = p_i; count = m_i}, {center = p_i1; count = m_i1 }, sum_i0 =
      find_i b 0 0.0 histogram.bins in
    let m_i = float m_i in
    let m_i1 = float m_i1 in
    let bpp = (b -. p_i) /. (p_i1 -. p_i) in
    let m_b = m_i +. (m_i1 -. m_i) *. bpp in
    let s = (m_i +. m_b) *. bpp /. 2. in
    s +. sum_i0 +. m_i /. 2.

let uniform =
  let rec loop span cum_sum_at_centers j accu =
    let s = (float j) *. span in
    match cum_sum_at_centers with
    | (cum_sum_0, {center = p_0; count = m_0}) ::
        ((cum_sum_1, {center = p_1; count = m_1}) as bin_1) :: rest ->
      if s < cum_sum_0 then
        loop span cum_sum_at_centers (j + 1) accu
      else if cum_sum_0 <= s && s < cum_sum_1 then
        let d = s -. cum_sum_0 in
        let c = -2. *. d in
        let b = float (2 * m_0) in
        let a = float (m_1 - m_0) in
        let z = pos_quadratic_root ~a ~b ~c in
        let u = p_0 +. (p_1 -. p_0) *. z in
        loop span cum_sum_at_centers (j + 1) ((j, u) :: accu)
      else
        loop span (bin_1 :: rest) j accu
    | [ _ ] -> List.rev accu
    | [] -> assert false
  in
  let cum_sum_at_centers hist =
    let bin, hist_rest, cum_sum =
      match hist with
      | ({count; _} as bin) :: rest -> bin, rest, (float count) /. 2.
      | _ -> raise Empty
    in
    let _, _, cum_sum_at_centers = List.fold_left (
      fun (cum_sum, prev_count, cum_sum_at_centers) ({count; _} as bin) ->
        let cum_sum = cum_sum +. (float (prev_count + count)) /. 2. in
        let cum_sum_at_centers = (cum_sum, bin) :: cum_sum_at_centers in
        cum_sum, count, cum_sum_at_centers
    ) (cum_sum, bin.count, [cum_sum, bin]) hist_rest in
    List.rev cum_sum_at_centers
  in
  fun hist b ->
    if b < 1 then
      raise (Invalid_argument "uniform")
    else
      let cum_sum_at_centers = cum_sum_at_centers hist.bins in
      let span = (float hist.total_count) /. (float b) in
      loop span cum_sum_at_centers 0 []


let mean { bins; total_count; _ } =
  if total_count = 0 then
    raise Empty
  else
    let m = List.fold_left (
      fun sum { center; count } ->
        sum +. center *. (float count)
    ) 0.0 bins in
    m /. (float total_count)

let mean_stdev histogram =
  if histogram.total_count = 0 then
    raise Empty
  else
    let mean = mean histogram in
    let v = List.fold_left (
        fun sum { center; count } ->
          let diff = center -. mean in
          sum +. diff *. diff *. (float count)
      ) 0.0 histogram.bins
    in
    let stdev = sqrt (v /. (float histogram.total_count)) in
    mean, stdev

