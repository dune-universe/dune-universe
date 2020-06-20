(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Settings
open CustomSek
open Benchmark
module Cmdline = Shared.Cmdline
let figure = Shared.figure

let m =
  Cmdline.parse_int "m"

let n =
  Cmdline.parse_int "n"

let k =
  n/m

let n =
  k * m

(* The benchmark. *)

let edummy : int E.t =
  E.create (-1)

let einit =
  E.init (-1)

let eeinit =
  E.init edummy

let eedummy : int E.t E.t =
  E.create edummy

let pdummy : int P.t =
  P.create (-1)

let pinit =
  P.init (-1)

let ppinit =
  P.init pdummy

let eflatten (ss : 'a E.t E.t) : 'a E.t =
  let result = E.create (E.default (E.default ss)) in
  E.iter forward (fun s ->
    E.append back result s
  ) ss;
  result

let benchmark =
  match Cmdline.parse_string "seq" with
  | "PSek" ->

      let name =
        sprintf "Flattening a family of %s persistent sequences of size %s (total size %s)"
          (figure m) (figure k) (figure n)
      and quota =
        Span.of_string "1.0s"
      and basis =
        m (* show cost per concat operation *)
      and run () =
        (* Allocate a family of [m] sequences of size [k]. *)
        let family : int P.t P.t =
          ppinit m (fun _ -> pinit k (fun i -> i))
        in
        fun () ->
          (* This is the timed section. *)
          let s = P.flatten family in
          sink (P.length s)
      in
      benchmark ~name ~quota ~basis ~run

  | "ESek" ->

      let name =
        sprintf "Flattening a family of %s ephemeral sequences of size %s (total size %s)"
          (figure m) (figure k) (figure n)
      and quota =
        Span.of_string (if n >= 1000000 then "5.0s" else "1.0s")
      and basis =
        m (* show cost per concat operation *)
      and run () =
        (* Allocate a family of [m] sequences of size [k]. *)
        let family : int E.t E.t =
          eeinit m (fun _ -> einit k (fun i -> i))
        in
        fun () ->
          (* This is the timed section. *)
          let s = eflatten family in
          sink (E.length s)
      in
      benchmark ~name ~quota ~basis ~run

  | seq ->
      invalid_arg (sprintf "seq = %s" seq)

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
