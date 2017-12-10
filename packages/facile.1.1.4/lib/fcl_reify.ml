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
(* $Id: fcl_reify.ml,v 1.21 2004/08/12 15:22:07 barnier Exp $ *)

open Fcl_var
module C = Fcl_cstr

let reification c b on_not =
  let name = "reification"
  and fprint s = Printf.fprintf s "reification: "; C.fprint s c
  and delay x =
    C.self_delay c x;
    if on_not then (C.self_delay (C.not c)) x;
    delay [Fd.on_subst] b x
  and update _0 =
    match Fd.value b with
      Val vb ->
	Fcl_cstr.post (if vb = 0 then (C.not c) else c);
	true
    | Unk _ ->
	try
	  if C.is_solved c || C.check c then begin
	    Fd.unify b 1
	  end else begin
	    Fd.unify b 0
	  end;
	  true
	with
	  Fcl_cstr.DontKnow -> false in
  C.create ~name ~fprint update delay

let cstr ?(delay_on_negation = true) c b =
  reification c b delay_on_negation;;

let boolean ?delay_on_negation c  =
  let b = Fd.create Fcl_domain.boolean in
  let r = cstr ?delay_on_negation c b in
  Fcl_cstr.post r;
  b;;

exception MyDontKnow;;

let rec (||~~) c1 c2 =
  let update _0 =
    C.is_solved c1 || C.is_solved c2 || 
    try
      if not (C.check c1) then begin (* if c1 is false, c2 must be true *)
	Fcl_cstr.post c2
      end;
      true
    with
      Fcl_cstr.DontKnow -> 
	try
	  if not (C.check c2) then (* if c2 is false, c1 must be true *)
	    Fcl_cstr.post c1;
	  true
	with
	  Fcl_cstr.DontKnow -> false

  and fprint s = Printf.fprintf s "("; C.fprint s c1; Printf.fprintf s ") ||~~ ("; C.fprint s c2; Printf.fprintf s ")"

  and delay c =
    C.self_delay c1 c;
    C.self_delay c2 c;
    C.self_delay (C.not c1) c;
    C.self_delay (C.not c2) c

  and check () =
    C.is_solved c1 || C.is_solved c2 ||
    try 
      (try C.check c1 with Fcl_cstr.DontKnow -> raise MyDontKnow) || C.check c2
    with
      MyDontKnow ->
	C.check c2 || raise Fcl_cstr.DontKnow

  and not () = (&&~~) (C.not c1) (C.not c2)

  in

  Fcl_cstr.create ~name:"||~~" ~fprint:fprint ~not:not ~check:check update delay

and (&&~~) c1 c2 =
  let update _ =
    Fcl_cstr.post c1;
    Fcl_cstr.post c2;
    true

  and fprint s = Printf.fprintf s "("; C.fprint s c1; Printf.fprintf s ") &&~~ ("; C.fprint s c2; Printf.fprintf s ")"

  and delay c =
    C.self_delay c1 c;
    C.self_delay c2 c;
    C.self_delay (C.not c1) c;
    C.self_delay (C.not c2) c

  and check () =
    (C.is_solved c1 ||
      try C.check c1 with
	Fcl_cstr.DontKnow ->
	  if C.check c2 then raise Fcl_cstr.DontKnow else false )
      &&
    (C.is_solved c2 || C.check c2)

  and not () = (||~~) (C.not c1) (C.not c2)

  in

  Fcl_cstr.create ~name:"&&~~" ~fprint:fprint ~not:not ~check:check update delay;;

let (=>~~) c1 c2 = C.not c1 ||~~ c2

let rec eq_or_xor c1 c2 equiv = (* if [equiv] then (<=>~~) else (xor~~) *)
  let update _0 =
    try
      Fcl_cstr.post (if C.check c1 = equiv then c2 else C.not c2);
      true
    with
      Fcl_cstr.DontKnow -> (* c1 unknown *)
	try
	  Fcl_cstr.post (if C.check c2 = equiv then c1 else C.not c1);
	  true
	with
	  Fcl_cstr.DontKnow -> (* c1 && c2 unknown *)
	    false

  and delay c =
    C.self_delay c1 c;
    C.self_delay c2 c;
    C.self_delay (C.not c1) c;
    C.self_delay (C.not c2) c
  and check _0 =
    (C.check c1 = C.check c2) = equiv

  and not () =
    eq_or_xor c1 c2 (not equiv)

  in

  Fcl_cstr.create ~name:(if equiv then "<=>~~" else "xor~~") ~not:not ~check:check update delay;;


let (<=>~~) c1 c2 = eq_or_xor c1 c2 true

let xor c1 c2 = eq_or_xor c1 c2 false

let not c = C.not c
