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
   toplevel loop. Open it under emacs and use Ctrl-C Ctrl-E to evaluate one
   command at a time in the OCaml REPL. *)

#use "topfind";;
#require "sek";;
open Sek;;

(* Defining and installing pretty-printers. *)
#require "pprint";;
(* [wrap] transforms a PPrint pretty-printer into a printer
   that can be installed by [#install_printer]. *)
let wrap (print : 'a -> PPrint.document) (channel : Format.formatter) (s : 'a) : unit =
  PPrint.ToFormatter.pretty 0.8 76 channel (print s);;
(* A printer for ephemeral sequences of integers. *)
let print_int_eseq =
  wrap (fun s -> PPrint.OCaml.flowing_list PPrint.OCaml.int (E.to_list s));;
#install_printer print_int_eseq;;
(* A printer for iterators on ephemeral sequences of integers. *)
let print_int_eiter =
  wrap (fun (it : int E.Iter.iter) ->
    if E.Iter.is_valid it then
      PPrint.OCaml.int (E.Iter.index it)
    else
      PPrint.utf8string "<invalid>"
  );;
#install_printer print_int_eiter;;
(* A printer for persistent sequences of integers. *)
let print_int_pseq =
  wrap (fun s -> PPrint.OCaml.flowing_list PPrint.OCaml.int (P.to_list s));;
#install_printer print_int_pseq;;
(* A printer for iterators on persistent sequences of integers. *)
let print_int_piter =
  wrap (fun (it : int P.Iter.iter) -> PPrint.OCaml.int (P.Iter.index it));;
#install_printer print_int_piter;;

(* Using Sek directly, with default settings. *)
let sum e = E.fold_left (+) 0 e;;
let e = E.of_array (-1) [|0;1;2;3|];;
assert (sum e = 6);;
let e' = E.copy e;;
E.set e' 0 42;;
assert (sum e = 6);;
assert (sum e' = 48);;

let p = P.of_array (-1) (Array.init 32 (fun i -> i));;
let sum = P.fold_left (+) 0 p;;
assert (sum = 496);;
let it = P.Iter.create forward p;;

(* Using the functor [Make] allows supplying custom settings. *)
module Settings = struct
  include DefaultSettings
  let capacity _depth = 8
  let overwrite_empty_slots = false
  let threshold = 2
end;;
module S = Make(Settings);;
open S;;
let e = E.of_array (-1) [|0;1;2;3|];;
let sum = E.fold_left (+) 0 e;;
assert (sum = 6);;
let p = P.of_array (-1) (Array.init 32 (fun i -> i));;
let sum = P.fold_left (+) 0 p;;
assert (sum = 496);;

(* Using the functor [SupplyDefault] allows supplying a default argument
   once and for all. Unfortunately, this requires choosing a fixed type
   of elements. *)
module Default = struct
  type element = int
  let default = -1
end;;
module S =
  SupplyDefault
    (Sek) (* using [S] here would work, too *)
    (Default)
;;
open S;;
let e = E.of_array [|0;1;2;3|];;
let sum = E.fold_left (+) 0 e;;
assert (sum = 6);;
let p = P.of_array (Array.init 32 (fun i -> i));;
let sum = P.fold_left (+) 0 p;;
assert (sum = 496);;

(* Using the List emulation layer. *)
module List = Emulated.List;;
let sum xs = List.fold_left (+) 0 xs;;
let xs = List.init 3 (fun i -> i);;
assert (sum xs = 3);;

(* Using the Stack emulation layer. *)
module Stack = Emulated.Stack;;
let s = Stack.create();;
let () = Stack.push 1 s;;
let () = Stack.push 2 s;;
let elements s = Stdlib.List.rev (Stack.fold (fun xs x -> x :: xs) [] s);;
let () = assert (elements s = [2; 1]);;
let () = assert (Stack.pop s = 2);;
let () = assert (Stack.pop s = 1);;
let () = assert (Stack.top_opt s = None);;

(* Using the Queue emulation layer. *)
module Queue = Emulated.Queue;;
let q = Queue.create();;
let () = Queue.add 1 q;;
let () = Queue.add 2 q;;
let elements q = Stdlib.List.rev (Queue.fold (fun xs x -> x :: xs) [] q);;
let () = assert (elements q = [1; 2]);;
let () = assert (Queue.take q = 1);;
let () = assert (Queue.take q = 2);;
let () = assert (Queue.peek_opt q = None);;
let q1 = Queue.create();;
let () = Queue.add 1 q1;;
let () = Queue.add 2 q1;;
let q2 = Queue.create();;
let () = Queue.add 3 q2;;
let () = Queue.add 4 q2;;
let () = Queue.transfer q1 q2;;
let () = assert (elements q1 = []);;
let () = assert (elements q2 = [3;4;1;2]);;
