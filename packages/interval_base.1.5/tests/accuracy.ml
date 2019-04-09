open Format

let singleton x_low x_high =
  if x_low = x_high then "= {•}" else "≠ {•}"

let () =
  let open Interval_intel.Fpu in
  let pi = acos(-1.) in
  printf "Fpu:\ncos(%.10f) ≈ [%.17f, %.17f] %s\n"
    pi (Low.cos pi) (High.cos pi) (singleton (Low.cos pi) (High.cos pi));
  printf "sin(%.10f) ≈ [%g, %e] %s\n"
    pi (Low.sin pi) (High.sin pi) (singleton (Low.sin pi) (High.sin pi))

let () =
  let open Interval_crlibm in
  let pi = acos(-1.) in
  printf "CRLibm:\ncos(%.10f) ∈ [%.17f, %.17f] %s\n"
    pi (Low.cos pi) (High.cos pi) (singleton (Low.cos pi) (High.cos pi));
  printf "sin(%.10f) ∈ [%g, %g] %s\n"
    pi (Low.sin pi) (High.sin pi) (singleton (Low.sin pi) (High.sin pi));
  let c = I.(cos pi) in
  printf ("cos([π]) = " ^^ I.fmt "%.17f" ^^ " %s\n")
    c (singleton c.low c.high);
  let c = I.(cospi one) in
  printf ("cospi([1,1]) = " ^^ I.fmt "%.17f" ^^ " %s\n")
    c (singleton c.low c.high);
  let s = I.(sin pi) in
  printf ("sin([π]) = " ^^ I.fmt "%g" ^^ " %s\n")
    s (singleton s.low s.high);
  let s = I.(sinpi one) in
  printf ("sinpi([1,1]) = " ^^ I.fmt "%g" ^^ " %s\n")
    s (singleton s.low s.high)
