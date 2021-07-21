include Biocaml_unix.Range

let relative_position x ~wrt:y =
  if x.lo < y.lo then (
    if x.hi < y.lo then `Before
    else if x.hi < y.hi then `Before_with_intersection
    else `Contains
  )
  else if x.lo = y.lo then (
    if x.hi < y.hi then `Included
    else if x.hi = y.hi then `Equal
    else `Contains
  )
  else if x.lo <= y.hi then (
    if x.hi <= y.hi then `Included
      else `After_with_intersection
    )
  else `After

let make ~lo ~hi =
  if lo > hi then raise (Invalid_argument "Range.make: lo > hi")
  else make_unsafe lo hi

let convex_hull p q =
  make_unsafe
    (min p.lo q.lo)
    (max p.hi q.hi)
