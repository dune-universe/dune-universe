(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: seven_eleven.ml,v 1.4 2004/09/07 13:41:28 barnier Exp $ *)

(*
   Arithmetic puzzle 711

   Find the numbers a, b, c and d such that

       a+b+c+d = 711 and a*b*c*d = 711000000

   This example was suggested by Mattias Waldau.
*)

open Facile
open Easy

let seven_eleven () =
  let a = Fd.interval 0 330 and b = Fd.interval 0 160
  and c = Fd.interval 0 140 and d = Fd.interval 0 140 in
  (* max a * max b * max c * max d should be less than max_int*)
  assert (Fd.max a * Fd.max b * Fd.max c * Fd.max d <= max_int);
  Cstr.post (fd2e a +~ fd2e b +~ fd2e c +~ fd2e d =~ i2e 711);
  Cstr.post (fd2e a *~ fd2e b *~ fd2e c *~ fd2e d =~ i2e 711000000);
  let numbers = [|a;b;c;d|] in
  if Goals.solve (Goals.Array.labeling numbers) then
    Printf.printf "%a\n" Fd.fprint_array numbers
  else
    prerr_endline "No solution"

let _ = seven_eleven ()
