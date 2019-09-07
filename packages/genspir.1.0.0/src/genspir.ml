(* Copyright (c) 2013, Zhang Initiative Research Unit,
* Advance Science Institute, RIKEN
* 2-1 Hirosawa, Wako, Saitama 351-0198, Japan
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
*
* Redistributions of source code must retain the above copyright notice,
* this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
* this list of conditions and the following disclaimer in the documentation
* and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
* HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *)

(* Based on:
   Saff, E.B. & Kuijlaars, A.B.J. (1997).
   The Mathematical Intelligencer, 19, 5-11,
   http://www.math.vanderbilt.edu/~esaff/texts/161.pdf *)

let pi = 4.0 *. atan 1.0

let h k n =
  -1. +. ((2. *. (k -. 1.)) /.
          (n -. 1.))

(* theta is in [0; pi] *)
let theta k n =
  acos (h k n)

(* psi is in [0; 2pi] *)
let psi k n psi_km1 =
  let hk  = h k n    in
  let hk2 = hk *. hk in
  mod_float (psi_km1 +. (3.6 /. ((sqrt n) *. (sqrt (1. -. hk2)))))
            (2. *. pi)

(* generalized spiral terms for k in [1..n] in spherical coordinates *)
let genspir_spherical n =
  let m = float_of_int n in
  let rec generalized_spiral_priv k acc =
    if k = m then
      ((theta k m, 0.) :: acc)
    else
      let psi_k = psi k m (snd (List.hd acc)) in
      generalized_spiral_priv (k +. 1.) ((theta k m, psi_k) :: acc)
  in
  if m < 1. then
    failwith (Printf.sprintf "generalized_spiral: invalid range: %f" m)
  else
    let first_term = [(theta 1. m, 0.)] in
    if m = 1. then
      first_term
    else
      generalized_spiral_priv 2. first_term

(* generalized spiral terms for k in [1..n] in cartesian coordinates *)
let genspir_cartesian n =
  (* spherical to cartesian *)
  let to_xyz (theta, psi) =
    let ct, st, cp, sp = cos theta, sin theta, cos psi, sin psi in
    (st *. cp, st *. sp, -.ct)
  in
  List.rev_map to_xyz (genspir_spherical n)
