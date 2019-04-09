open Printf

let pi = acos(-1.)

let () =
  let open Crlibm in
  printf "cos(%.17f) ≈ %g (rounded to the nearest)\n" pi (cos pi);
  let cos_low = Low.cos pi and cos_high = High.cos pi in
  printf "cos(%.17f) ∈ [%.17f, %.17f]\n" pi cos_low cos_high;
  printf "                           (width: %g)\n" (cos_high -. cos_low);
  printf "cos(π) =: cospi(1.) ∈ [%g, %g]\n" (Low.cospi 1.) (High.cospi 1.);
  printf "acos(-1)/π =: acospi(-1.) = %g\n" (acospi (-1.))

let () =
  let open Crlibm in
  let exp_low = Low.exp (-1.) and exp_high = High.exp (-1.) in
  printf "exp(-1.) ∈ [%.17f, %.17f]\n" exp_low exp_high;
  printf "           (width: %g)\n" (exp_high -. exp_low);
