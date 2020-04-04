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
   toplevel environment. It can be executed by typing [ocaml play.ml].
   Do not forget to install the library first, e.g., [make reinstall]. *)
#use "topfind";;
#require "sek";;
open Sek;;

(* Using the default instantiation directly. *)
#install_printer E.format;;
#install_printer P.format;;
let e = E.of_array (-1) [|0;1;2;3|];;

(* Using the functor [Make]. *)
module C = struct let capacity _ = 2 end;;
module O = DoOverwriteEmptySlots;;
module T = struct let threshold = 2 end;;
module S = Make(C)(O)(T);;
open S;;
#install_printer E.format;;
#install_printer P.format;;
let e = E.of_array (-1) [|0;1;2;3|];;
