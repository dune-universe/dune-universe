(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: reduce.ml,v 1.1 2006/11/28 15:43:28 rousse Exp $*)

open Color
open Rgb24

module type REDUCER = sig
  val find_nearest : Color.rgb Color.map -> Color.rgb -> int
end


(* Error diffusion weight table *)
let diffusion_table =
  let sum = 1.0 +. 1.0 +. 1.0 /. sqrt 2.0 in
  let base = 1.0 /. sum in
  [| [| 0.0 ; base |];
     [| base; base /. (sqrt 2.0) |] |]

(* Reduce colors to the given colormap (<= 256 colors) using error diffusion *)
module ErrorDiffuse( R : REDUCER ) = struct
  let f src colormap =
    if colormap.max > 256 then
      raise (Invalid_argument "Rgb24.to_index8: too large colormap");
    let error_table =
      Array.init 2
        (fun _ ->
           Array.init (src.width + 1) (fun _ -> {r = 0; g = 0; b = 0})) in
    let get_error x y =
      let y' = y mod 2 in error_table.(y').(x) in
    let add_error x y rgb =
      let y' = y mod 2 in
      let rgb' = error_table.(y').(x) in
      error_table.(y').(x) <- Color.plus rgb rgb' in
    let next_line y =
      (* reset the error table of the current line *)
      let y' = y mod 2 in
      for x = 0 to src.width do
          error_table.(y').(x) <- {r=0; g=0; b=0}
      done in
    let id8 = Index8.create (src.width) (src.height) in
    id8.Index8.colormap <- colormap;

    for y = 0 to src.height - 1 do
      for x = 0 to src.width -1 do
        let ideal_rgb = Color.plus (unsafe_get src x y) (get_error x y) in
        let c = R.find_nearest colormap ideal_rgb in
        Index8.unsafe_set id8 x y c;
        let new_error = Color.minus ideal_rgb colormap.map.(c) in
        for ey = 0 to 1 do
          for ex = 0 to 1 do
            let diffuse_rgb = {
              r= truncate (float new_error.r *. diffusion_table.(ex).(ey));
              g= truncate (float new_error.g *. diffusion_table.(ex).(ey));
              b= truncate (float new_error.b *. diffusion_table.(ex).(ey)); } in
            add_error (x+ex) (y+ey) diffuse_rgb
          done
        done
      done;
      next_line y
    done;
    id8
end

module ErrorDiffuseIndex8 = ErrorDiffuse(Color.Rgb)

let error_diffuse = ErrorDiffuseIndex8.f
