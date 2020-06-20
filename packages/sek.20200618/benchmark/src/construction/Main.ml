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

let n =
  Cmdline.parse_int "n"

(* The benchmark. *)

let quota =
  Span.of_string (
    if n < 1000000 then "1.0s"
    else if n < 10000000 then "2.0s"
    else "5.0s"
  )

let benchmark =
  match Cmdline.parse_string "seq", Cmdline.parse_string "op" with
  | "Array", "init" ->
      let name =
        sprintf "Init an array of size %s"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let a = Array.init n (fun i -> i) in
        sink (Array.length a)
      in
      benchmark ~name ~quota ~basis ~run
  | "Array", "make" ->
      let name =
        sprintf "Make an array of size %s"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let a = Array.make n 0 in
        sink (Array.length a)
      in
      benchmark ~name ~quota ~basis ~run
  | "ESek", "push" ->
      let name =
        sprintf "A series of %s push operations on an empty ephemeral sequence"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let s = E.create (-1) in
        for i = 1 to n do
          E.push front s i
        done;
        sink (E.length s)
      in
      benchmark ~name ~quota ~basis ~run
  | "ESek", "init" ->
      let name =
        sprintf "Init an ephemeral sequence of size %s"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let s = E.init (-1) n (fun i -> i) in
        sink (E.length s)
      in
      benchmark ~name ~quota ~basis ~run
  | "ESek", "make" ->
      let name =
        sprintf "Make an ephemeral sequence of size %s"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let s = E.make (-1) n 0 in
        sink (E.length s)
      in
      benchmark ~name ~quota ~basis ~run
  | "PSek", "push" ->
      let name =
        sprintf "A series of %s push operations on an empty persistent sequence"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        (* The reference should be optimised away. *)
        let s = ref (P.create (-1)) in
        for i = 1 to n do
          s := P.push front !s i
        done;
        sink (P.length !s)
      in
      benchmark ~name ~quota ~basis ~run
  | "PSek", "init" ->
      let name =
        sprintf "Init a persistent sequence of size %s"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let s = P.init (-1) n (fun i -> i) in
        sink (P.length s)
      in
      benchmark ~name ~quota ~basis ~run
  | "PSek", "make" ->
      let name =
        sprintf "Make a persistent sequence of size %s"
          (figure n)
      and basis =
        n
      and run () () =
        (* This is the timed section. *)
        let s = P.make (-1) n 0 in
        sink (P.length s)
      in
      benchmark ~name ~quota ~basis ~run
  | seq, op ->
      invalid_arg (sprintf "seq = %s, op = %s" seq op)

let () =
  if dry then
    run_once benchmark
  else
    drive_and_print benchmark
