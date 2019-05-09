open Printf
open Interval

let rnd_values = Array.init 1000 (fun _ -> 1000. -. Random.float 2000.)

let speed_cmp1 loops (name, f) =
  let l = Array.length rnd_values in
  printf "%12s speed (%d calls): %!" name loops;
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to loops / l do
    Array.iter (fun x -> ignore (f x)) rnd_values
  done;
  let dt = (Unix.times ()).Unix.tms_utime -. top in
  printf "%f\n%!" dt

let speed_cmp2 loops (name, f) =
  let l = Array.length rnd_values in
  printf "%12s speed (%d calls): %!" name loops;
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to loops / l do
    Array.iter (fun x -> ignore (f x x)) rnd_values;
  done;
  let dt = (Unix.times ()).Unix.tms_utime -. top in
  printf "%f\n%!" dt

let rnd_values_I =
  Array.init 1000 (fun _ ->
      let x1 = 1000. -. Random.float 2000. and x2 = Random.float 10. in
      I.v x1 (x1 +. x2))

let rnd_values_pos_I =
  Array.init 1000 (fun _ ->
      let x1 = Random.float 2000. and x2 = Random.float 10. in
      I.v x1 (x1 +. x2))

let speed_cmp1_I ?(pos=false) loops (name, f) =
  let l = Array.length rnd_values_I in
  printf "%12s speed (%d calls): %!" name loops;
  let rnd_I = if pos then rnd_values_pos_I else rnd_values_I in
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to loops / l do
    Array.iter (fun x -> ignore (f x)) rnd_I
  done;
  let dt = (Unix.times ()).Unix.tms_utime -. top in
  printf "%f\n%!" dt

let speed_cmp2_I loops (name, f) =
  let l = Array.length rnd_values_I in
  printf "%12s speed (%d calls): %!" name loops;
  let top = (Unix.times ()).Unix.tms_utime in
  for _ = 1 to loops / l do
    Array.iter (fun x -> ignore (f x x)) rnd_values_I
  done;
  let dt = (Unix.times ()).Unix.tms_utime -. top in
  printf "%f\n%!" dt

let () =
  let open Interval_intel in
  List.iter (speed_cmp1 10000000) [
      ("tan", tan); ("cos", cos); ("sin", sin);
      ("exp", exp); ("log", log);
      ("ftan", Fpu.ftan); ("fcos", Fpu.fcos); ("fsin", Fpu.fsin);
      ("fexp", Fpu.fexp); ("flog", Fpu.flog) ];

  List.iter (speed_cmp2 10000000) [
      ("+.", ( +. )); ("-.", ( -. )); ("*.", ( *. )); ("/.", ( /. ));
      ("**", ( ** )); ("mod_float", mod_float);
      ("fadd", Fpu.fadd); ("fsub", Fpu.fsub);
      ("fmul", Fpu.fmul); ("fdiv", Fpu.fdiv);
      ("fpow", Fpu.fpow);
      ("fmod", Fpu.fmod)];

  List.iter (speed_cmp2_I 10000000) [
      ("I+I", I.( + )); ("I-I", I.( - ));
      ("I*I", I.( * )); ("I/I", I.( / ))];

  List.iter (speed_cmp1_I 10000000) [
      ("Intel I.tan", I.tan); ("I.cos", I.cos); ("I.sin", I.sin);
      ("I.exp", I.exp); ];
  List.iter (speed_cmp1_I 10000000 ~pos:true) [
      ("I.log", I.log) ];

  let open Interval_crlibm in
  List.iter (speed_cmp1_I 10000000) [
      ("CRlibm I.tan", I.tan); ("I.cos", I.cos); ("I.sin", I.sin);
      ("I.exp", I.exp); ("I.expm1", I.expm1);];
  List.iter (speed_cmp1_I 10000000 ~pos:true) [
      ("I.log", I.log); ("I.log1p", I.log1p) ];
