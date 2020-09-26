(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

let dim = 3

type point = { x: float; y: float; m: float }

let a2p = fun p -> { x = p.(0); y = p.(1); m = p.(2) }

type bbox = { xmin: float; xmax: float; ymin: float; ymax: float;
	      mmin: float; mmax: float }

let a2b = fun b ->
  { xmin = b.(0); xmax = b.(2); ymin = b.(1); ymax = b.(3);
    mmin = b.(6); mmax = b.(7) }

let print_bbox = fun b ->
  Printf.printf "xmin = %f, xmax = %f\n" b.xmin b.xmax;
  Printf.printf "ymin = %f, ymax = %f\n" b.ymin b.ymax;
  Printf.printf "mmin = %f, mmax = %f\n" b.mmin b.mmax
