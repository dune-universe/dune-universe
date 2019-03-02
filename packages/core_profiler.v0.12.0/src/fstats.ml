(** This module is basically copied straight from [Jane.Rstats], however:
      - [decay] removed (to avoid Option.value branch in [update_in_place])
      - [update_in_place] optimised so that it doesn't allocate (see below)

    This copy can be killed when the original is available publicly. *)

open Core

type t = {
  (* Note: we keep samples as a float instead of an int so that all floats in the record
     are kept unboxed. *)
  mutable samples : float;            (* sum of sample^0 *)
  mutable sum: float;                 (* sum of sample^1 *)
  mutable sqsum: float;               (* sum of sample^2 *)

  (* The naive algorithm of (sqsum - sum^2) exhibits catastrophic cancellation,
     so we use an alternative algorithm that keeps a running total of the variance
     and gets much better numerical precision.

     See "Weighted incremental algorithm" and "Parallel algorithm" at
     http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  *)
  mutable varsum : float;              (* sum of running_variance *)

  mutable max : float;                (* largest sample *)
  mutable min : float;                (* smallest sample *)
}

let create () =
  { sum     = 0.
  ; sqsum   = 0.
  ; varsum  = 0.
  ; samples = 0.
  ; max = Float.neg_infinity
  ; min = Float.infinity
  }

let samples t = Float.to_int t.samples
let total   t = t.sum
let min     t = t.min
let max     t = t.max

let mean t = t.sum /. t.samples

let var t =
  if t.samples <= 1.
  then Float.nan
  else Float.max 0. ((t.varsum /. t.samples) *. (t.samples /. (t.samples -. 1.)))

let stdev t = sqrt (var t)

let update_in_place t value =
  if t.samples <= 0.
  then begin
    (* [Rstats.safe_mean] allocates, even after it's been inlined.
       It seems that in general, expressions of form
       [if expr then some_float else other_float]
       cause allocation, even in cases where it is unnecessary.

       So, I handle the [t.samples <= 0] separately, so that [safe_mean] is
       not needed. *)
    t.samples <- 1.;
    t.sum <- value;
    t.sqsum <- value *. value;
    t.max <- value;
    t.min <- value;
    t.varsum <- 0.
  end
  else begin
    (* [Option.value decay ~default_decay:...] suffers the same problem. *)
    let old_mean = mean t in
    t.samples <- t.samples +. 1.;
    t.sum     <- t.sum     +. value;
    t.sqsum   <- t.sqsum   +. value *. value;
    (* [Float.max] suffers the same problem as above and causes allocation even
       after it's been inlined. *)
    if value > t.max then t.max <- value;
    if value < t.min then t.min <- value;
    let new_mean = mean t in
    (* Numerically stable method for computing variance.  On wikipedia page:
       # Alternatively, "M2 = M2 + weight * delta * (x−mean)" *)
    t.varsum <- t.varsum +. (value -. old_mean) *. (value -. new_mean)
  end

let copy t = { t with sum = t.sum }

let%test_module "Fstats" = (module struct
  let data =
    [|198215.02492520443; 28715.0284862834887; 434619.094190333097; 800678.330169520807
     ; 200186.54372400351; 137503.258498826239; 566498.60534151; 549219.475914424169
     ; 780230.679805230233; 712168.552241884521; 512045.175157501479
     ; 606136.851468109642; 368469.614224194782; 213372.100528741139
     ; 487759.722525204881; 545327.353652161313; 565759.781024767901
     ; 227130.713477647136; 14526.9831253076572; 87168.3680568782729
     ; 317822.864412072755; 328746.783061697963; 446049.964182617899
     ; 451270.307992378599; 822506.373272555647; 947812.349198815064
     ; 563960.680863914196; 73057.735605084; 475515.868111219897; 79103.4644861585693
     ; 61060.2804668050376; 821842.058883985155; 162383.377334053017
     ; 151034.116153264389; 357173.747924180352; 417551.514353964303
     ; 758440.286012416356; 480593.8769512953; 109763.567296876703
     ; 154961.955787061859; 50902.5130336880611; 273048.455977052858
     ; 673477.375928052119; 790059.438551203464; 817997.314074333408
     ; 280563.866073483776; 858501.471649471; 908670.036968784756; 843433.873243822
     ; 717604.357264731894; 257166.21131005112; 587352.255237122881; 679376.01970596856
     ; 93196.2210949568544; 343319.788271304453; 757660.644341278588
     ; 403271.576879935688; 974099.221967302146; 964390.741413959884
     ; 807222.013931629714; 670868.156459537; 656612.853921575472; 545398.269980843412|]

  let stats = create ()
  let () =
    Array.iter data ~f:(update_in_place stats)

  let%test "samples"  = samples stats = 63
  let%test "min"      = min   stats = 14526.9831253076572
  let%test "max"      = max   stats = 974099.221967302146
  let%test "total"    = total stats = 29970575.106168244
  let%test "mean"     = mean  stats = 475723.414383622934
  let%test "var"      = var   stats = 78826737609.7966156
  let%test "stdev"    = stdev stats = 280760.997308736958

  let%test_unit "copy" =
    let stats2 = copy stats in
    update_in_place stats2 0.;
    [%test_eq: int] (samples stats) 63;
    [%test_eq: int] (samples stats2) 64
end)

let%bench_module "Fstats" = (module struct
  let stats = create ()

  let%bench "update_in_place" = update_in_place stats 5.
end)
