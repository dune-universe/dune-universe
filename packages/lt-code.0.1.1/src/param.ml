type t = {
  systematic : bool;
  systematic_scaling_factor : float;
  data_block_count : int;
  max_drop_count : int;
  dist : Dist.t;
}

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  | `Invalid_systematic_scaling_factor
  ]

let systematic t = t.systematic

let systematic_scaling_factor t = t.systematic_scaling_factor

let data_block_count t = t.data_block_count

let max_drop_count t = t.max_drop_count

let dist t = t.dist

let make ?(systematic_scaling_factor = 10.0) ~systematic ~data_block_count
    ~max_drop_count () : (t, error) result =
  if data_block_count <= 0 || data_block_count > Constants.max_data_block_count
  then Error `Invalid_data_block_count
  else if
    max_drop_count < data_block_count
    || max_drop_count > Constants.max_drop_count
  then Error `Invalid_drop_count
  else if systematic_scaling_factor <= 0.0 then
    Error `Invalid_systematic_scaling_factor
  else
    Ok
      {
        systematic;
        systematic_scaling_factor;
        data_block_count;
        max_drop_count;
        dist = Dist.robust_soliton_dist ~k:data_block_count;
      }
