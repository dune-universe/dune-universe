exception Invalid_interval

module Make (Bound : Bound.S) = struct
  open Bound

  type t =
    { low : Bound.t
    ; high : Bound.t
    }

  let is_valid_bounds low high =
    match low, high with
    | Included low, Included high ->
      low <= high
    | Included low, Excluded high
    | Excluded low, Included high
    | Excluded low, Excluded high ->
      low < high
    | _ ->
      true

  let create low high =
    if is_valid_bounds low high then
      { low; high }
    else
      raise Invalid_interval

  let is_valid { low; high } = is_valid_bounds low high

  let low_bound_compare a b = Bound.compare_lower a.low b.low

  let high_bound_compare a b = Bound.compare_upper a.high b.high

  let compare a b =
    let low_bound_cmp = low_bound_compare a b in
    if low_bound_cmp = 0 then
      high_bound_compare a b
    else
      low_bound_cmp

  let overlap_low_high ivl_a ivl_b =
    let low =
      if low_bound_compare ivl_a ivl_b < 0 then
        ivl_b.low
      else
        ivl_a.low
    in
    let high =
      if high_bound_compare ivl_a ivl_b < 0 then
        ivl_a.high
      else
        ivl_b.high
    in
    low, high

  let overlap_interval ivl_a ivl_b =
    let low, high = overlap_low_high ivl_a ivl_b in
    let interval = { low; high } in
    if is_valid interval then
      Some interval
    else
      None

  let overlaps ivl_a ivl_b =
    let low, high = overlap_low_high ivl_a ivl_b in
    is_valid_bounds low high
end
