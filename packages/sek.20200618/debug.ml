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

(* This OCaml script shows how one can play with sequences in the OCaml
   toplevel environment. It can be executed by typing [ocaml debug.ml].
   Do not forget to install the library first. *)
#use "topfind";;
#require "sek";;
open Sek;;

(* This line helps replay scenarios produced by the fuzzer. *)
#use "fuzz/src/Misc.ml";;

(* Install printers that produce debugging output. *)
#install_printer E.format;;
#install_printer P.format;;
#install_printer P.Iter.format;;
#install_printer E.Iter.format;;

(* Using Sek directly, with default settings. *)
let e = E.of_array (-1) [|0;1;2;3|];;
let sum = E.fold_left (+) 0 e;;
assert (sum = 6);;
let p = P.of_array (-1) (Array.init 32 (fun i -> i));;
let sum = P.fold_left (+) 0 p;;
assert (sum = 496);;
