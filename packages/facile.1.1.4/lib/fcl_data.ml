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
module Array = struct
  let set t i v =
    let old = t.(i) in
    t.(i) <- v; Fcl_stak.trail (fun () -> t.(i) <- old)
end

module Hashtbl = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  let create n = Hashtbl.create n
  let get h = h

  let add h k d =
    Hashtbl.add h k d;
    Fcl_stak.trail (fun () -> Hashtbl.remove h k)
  let remove h k =
    let d = Hashtbl.find h k in
    Hashtbl.remove h k;
    Fcl_stak.trail (fun () -> Hashtbl.add h k d)
  let find = Hashtbl.find
  let replace h k d =
    let od = Hashtbl.find h k in
    Hashtbl.replace h k d;
    Fcl_stak.trail (fun () -> Hashtbl.replace h k od)
  let mem = Hashtbl.mem
  let iter = Hashtbl.iter
  let fold = Hashtbl.fold
end
