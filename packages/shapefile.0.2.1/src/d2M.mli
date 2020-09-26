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

(** Types for 2D + value (x, y, m) shapes *)

type point = { x: float; y: float; m: float }
type bbox = { xmin: float; xmax: float; ymin: float; ymax: float;
	      mmin: float; mmax: float }

val print_bbox : bbox -> unit

(**/**)
val dim: int
val a2p: float array -> point
val a2b: float array -> bbox
