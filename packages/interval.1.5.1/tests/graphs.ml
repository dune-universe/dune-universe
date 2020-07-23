(* Simple graphical tests (using TikZ/LaTeX).  These tests will only
   show gross problems in the implementation. *)

open Printf
open Interval_crlibm

let eval_intervals fname ?(fill=true) ?map_x f xs =
  let map_x = match map_x with Some f -> f | None -> (fun x -> x) in
  let fh = open_out fname in
  let args = if fill then "fill" else "" in
  let write_interval x =
    let y = f x in
    fprintf fh "\\draw[%s] (%f, %f) rectangle (%f, %f);\n" args
      (map_x x.low) y.low (map_x x.high) y.high in
  List.iter write_interval xs;
  close_out fh

(* Partition into [n] sub-intervals *)
let partition a b n =
  if a >= b then failwith "partition: a < b";
  let l = ref [] in
  let dx = (b -. a) /. float n in
  let x1 = ref b in
  for i = n - 1 downto 0 do
    (* We use float computations.  Errors do not matter much because
       we use the same bound for two consecutive intervals. *)
    let x0 = a +. float i *. dx in
    l := I.v x0 !x1 :: !l;
    x1 := x0;
  done;
  !l

let sample fname ?map_x f a b ~n =
  let map_x = match map_x with Some f -> f | None -> (fun x -> x) in
  let fh = open_out fname in
  let h = (b -. a) /. float(n - 1) in
  for i = 0 to n - 1 do
    let x = a +. float i *. h in
    fprintf fh "%g %e\n" (map_x x) (f x)
  done;
  close_out fh

let () =
  let a = 1.4 and b = 1.7 in
  let x = partition a b 30 in
  eval_intervals "sin.tex" I.sin x;
  sample "sin.dat" sin a b ~n:1000

let () =
  let a = 1000. and b = 1014. in
  let map_x x = x -. 1000. in
  eval_intervals "sin2.tex" I.sin (partition a b 8) ~map_x;
  eval_intervals "sin3.tex" I.sin (partition a b 20) ~map_x;
  sample "sin2.dat" sin a b ~n:1000 ~map_x
