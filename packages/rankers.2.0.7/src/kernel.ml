(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

(* cf. https://en.wikipedia.org/wiki/Kernel_(statistics) for graphics
   and a list of commonly used kernels.
   Here we only implement vanishing kernels; i.e. they have a bounded
   support (for computational efficiency). *)
type flavor = Uniform
            | Triangle
            | Epanechnikov
            | Biweight
            | Triweight
            | Tricube

let of_string = function
  | "uni" -> Uniform
  | "tri" -> Triangle
  | "epa" -> Epanechnikov
  | "biw" -> Biweight
  | "trw" -> Triweight
  | "trc" -> Tricube
  | s -> failwith ("Kernel.of_string: unsupported: " ^ s)

let to_string = function
  | Uniform      -> "uni"
  | Triangle     -> "tri"
  | Epanechnikov -> "epa"
  | Biweight     -> "biw"
  | Triweight    -> "trw"
  | Tricube      -> "trc"

let pow2 x =
  x *. x

let pow3 x =
  x *. x *. x

let uniform x =
  if x <= 1.0
  then 0.5
  else 0.0

let triangle x =
  if x <= 1.0
  then 1.0 -. x
  else 0.0

let epanechnikov x =
  if x <= 1.0
  then 0.75 *. (1.0 -. (pow2 x))
  else 0.0

let biweight x =
  if x <= 1.0 then
    (* 0.9375 = 15/16 *)
    0.9375 *. pow2 (1.0 -. (pow2 x))
  else 0.0

let triweight x =
  if x <= 1.0 then
    (* 1.09375 = 35/32 *)
    1.09375 *. pow3 (1.0 -. (pow2 x))
  else 0.0

let tricube x =
  if x <= 1.0 then
    (70. /. 81.) *. pow3 (1.0 -. (pow3 x))
  else 0.0

(* evaluate normalized kernel *)
let eval flavor bwidth x =
  assert(x >= 0.0 && x <= 1.0); (* x is a normalized distance *)
  let scale = 1.0 /. bwidth in
  let x' = x /. bwidth in
  match flavor with
  | Uniform ->      scale *. (uniform x')
  | Triangle ->     scale *. (triangle x')
  | Epanechnikov -> scale *. (epanechnikov x')
  | Biweight ->     scale *. (biweight x')
  | Triweight ->    scale *. (triweight x')
  | Tricube ->      scale *. (tricube x')

(* write kernel values to file, for gnuplot and to check the given kernel
   is properly implemented *)
let to_file flavor bwidth fn =
  Utls.with_out_file fn (fun out ->
      let xs = BatList.frange 0.0 `To 1.0 101 in
      BatList.iter (fun x ->
          let y = eval flavor bwidth x in
          Printf.fprintf out "%f %f\n" x y
        ) xs
    )
