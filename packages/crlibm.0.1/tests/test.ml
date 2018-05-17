open Printf

let pi = acos(-1.)

let () =
  let open Crlibm in
  printf "cos(π) ≈ %g\n" (cos pi);
  let cos_low = Low.cos pi and cos_high = High.cos pi in
  printf "cos(%.17f) ∈ [%.17f, %.17f]\n" pi cos_low cos_high;
  printf "                           (width: %g)\n" (cos_high -. cos_low);
  printf "cos(π) ∈ [%g, %g]\n" (Low.cospi 1.) (High.cospi 1.)

let () =
  printf "acospi(-1) = %g\n" (Crlibm.acospi (-1.))
