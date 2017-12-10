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
(* $Id: marriage.ml,v 1.5 2004/09/07 13:40:26 barnier Exp $ *)
(*
  From ILOG stablema.cpp, OPL marriage.mod
*)

open Printf
open Facile
open Easy

let n = 5
let richard = 0 and james = 1 and john = 2 and hugh = 3 and greg = 4
let men = [| richard ; james ; john ; hugh ; greg |]
let helen = 0 and tracy = 1 and linda = 2 and sally = 3 and wanda = 4
let women = [| helen ; tracy ; linda ; sally ; wanda |]

let rankWomen =
  [|[|1; 2; 4; 3; 5 |];
    [|3; 5; 1; 2; 4 |];
    [|5; 4; 2; 1; 3 |];
    [|1; 3; 5; 4; 2 |];
    [|4; 2; 3; 5; 1 |]
  |];;

let rankMen =
  [|
  [|5; 1; 2; 4; 3 |];
  [|4; 1; 3; 2; 5 |];
  [|5; 3; 2; 4; 1 |];
  [|1; 5; 4; 3; 2 |];
  [|4; 3; 2; 1; 5 |]
|];;

let ai2e = Array.map i2e

let go () =
  let one_wife _ = Fd.create (Domain.interval 0 (Array.length women - 1))
  and one_husband _ = Fd.create (Domain.interval 0 (Array.length men - 1)) in
  let wife = Fd.array n 0 (n-1)
  and husband = Fd.array n 0 (n-1) in
  let wifee = Array.map fd2e wife
  and husbande = Array.map fd2e husband in

  Array.iter (fun m ->
    Cstr.post (fd2e (FdArray.get husband (Array.get wife m)) =~ i2e m)) men;
  Array.iter (fun w ->
    Cstr.post (fd2e (FdArray.get wife (Array.get husband w)) =~ i2e w)) women;

  let array_fd = Array.map Fd.int in
  let rankMen_wife =
    Array.map (fun m -> FdArray.get (array_fd rankMen.(m)) wife.(m)) men
  and rankWomen_husband =
    Array.map
      (fun w -> FdArray.get (array_fd rankWomen.(w)) husband.(w)) women in

  Array.iter
    (fun m ->
      Array.iter
	(fun w ->
	  let rankMen_m_w = i2e rankMen.(m).(w)
	  and rankMen_wife_m = fd2e rankMen_wife.(m)
	  and rankWomen_husband_w = fd2e rankWomen_husband.(w)
	  and rankWomen_w_m = i2e rankWomen.(w).(m) in
	  Cstr.post
	    (fd2e (Reify.boolean (rankMen_m_w <~ rankMen_wife_m)) <=~
	     fd2e (Reify.boolean (rankWomen_husband_w <~ rankWomen_w_m )));
	  Cstr.post
	    (fd2e (Reify.boolean (rankWomen_w_m <~ rankWomen_husband_w)) <=~
	     fd2e (Reify.boolean (rankMen_wife_m <~ rankMen_m_w))))
	women)
    men;

  let goal = Goals.Array.labeling wife &&~ Goals.Array.labeling husband in

  if Goals.solve goal then begin
    printf "wifes: "; Array.iter (fun w -> printf "%a " Fd.fprint w) wife;
    print_newline()
  end else
    prerr_endline "No"
  
let _ =
  go ();;
