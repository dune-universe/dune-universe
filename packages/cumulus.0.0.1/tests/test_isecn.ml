(* Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

module Int_set = Set.Make (Int)

type patch = [`Add of int | `Remove of int]

type patch2 = [
  | `Add1 of int
  | `Remove1 of int
  | `Add2 of int * int
  | `Remove2 of int * int
  | `Replace of int * int
]

let csA, send_csA = Cumulus.create Int_set.empty
let csB, send_csB = Cumulus.create Int_set.empty

(* It is assumed (in patch) that any patches had an effect on the set it is
 * paired with, i.e. that it did not add a present element or remove a missing
 * element. *)
let csAB : (Int_set.t, patch2) Cumulus.t =
  let init sA sB = Int_set.inter sA sB in
  let patch (sA, dsA) (sB, dsB) sAB =
    let keep () =
      Cumulus.Keep sAB in
    let add1 x =
      Cumulus.Patch (Int_set.add x sAB, `Add1 x) in
    let remove1 x =
      Cumulus.Patch (Int_set.remove x sAB, `Remove1 x) in
    let add2 x y =
      Cumulus.Patch (Int_set.add x (Int_set.add y sAB), `Add2 (x, y)) in
    let remove2 x y =
      Cumulus.Patch (Int_set.remove x (Int_set.remove y sAB), `Remove2 (x, y)) in
    let replace x y =
      Cumulus.Patch (Int_set.add y (Int_set.remove x sAB), `Replace (x, y)) in
    (match Option.value ~default:`Noop dsA, Option.value ~default:`Noop dsB with
     | `Noop, `Noop -> Cumulus.Keep sAB
     | `Add xA, `Noop -> if Int_set.mem xA sB then add1 xA else keep ()
     | `Noop, `Add xB -> if Int_set.mem xB sA then add1 xB else keep ()
     | `Remove xA, `Noop -> if Int_set.mem xA sB then remove1 xA else keep ()
     | `Noop, `Remove xB -> if Int_set.mem xB sA then remove1 xB else keep ()
     | `Add xA, `Add xB ->
        if xA = xB then add1 xA else
        (match Int_set.mem xA sB, Int_set.mem xB sA with
         | false, false -> keep ()
         | false, true -> add1 xB
         | true, false -> add1 xA
         | true, true -> add2 xA xB)
     | `Remove xA, `Remove xB ->
        if xA = xB then remove1 xA else
        (match Int_set.mem xA sB, Int_set.mem xB sA with
         | false, false -> keep ()
         | false, true -> remove1 xB
         | true, false -> remove1 xA
         | true, true -> remove2 xA xB)
     | `Add xA, `Remove xB ->
        if xA = xB then keep () else
        (match Int_set.mem xA sB, Int_set.mem xB sA with
         | false, false -> keep ()
         | false, true -> remove1 xB
         | true, false -> add1 xA
         | true, true -> replace xB xA)
     | `Remove xA, `Add xB ->
        if xA = xB then keep () else
        (match Int_set.mem xA sB, Int_set.mem xB sA with
         | false, false -> keep ()
         | false, true -> add1 xB
         | true, false -> remove1 xA
         | true, true -> replace xA xB))
  in
  Cumulus.l2 ~init ~patch csA csB

let test n =
  let rec loop n_it sA sB = if n_it > 0 then begin
    assert (Int_set.equal (Int_set.inter sA sB) (Cumulus.value csAB));
    (match Random.int 8 with
     | 0 ->
        let xA = Random.int n in
        let sA = Int_set.add xA sA in
        send_csA (Cumulus.Patch (sA, `Add xA));
        loop (n_it - 1) sA sB
     | 1 ->
        let xA = Random.int n in
        let sA = Int_set.remove xA sA in
        send_csA (Cumulus.Patch (sA, `Remove xA));
        loop (n_it - 1) sA sB
     | 2 ->
        let xB = Random.int n in
        let sB = Int_set.add xB sB in
        send_csB (Cumulus.Patch (sB, `Add xB));
        loop (n_it - 1) sA sB
     | 3 ->
        let xB = Random.int n in
        let sB = Int_set.remove xB sB in
        send_csB (Cumulus.Patch (sB, `Remove xB));
        loop (n_it - 1) sA sB
     | 4 ->
        let xA, xB = Random.int n, Random.int n in
        let sA, sB = Int_set.add xA sA, Int_set.add xB sB in
        let step = React.Step.create () in
        send_csA ~step (Cumulus.Patch (sA, `Add xA));
        send_csB ~step (Cumulus.Patch (sB, `Add xB));
        React.Step.execute step;
        loop (n_it - 1) sA sB
     | 5 ->
        let xA, xB = Random.int n, Random.int n in
        let sA, sB = Int_set.add xA sA, Int_set.remove xB sB in
        let step = React.Step.create () in
        send_csA ~step (Cumulus.Patch (sA, `Add xA));
        send_csB ~step (Cumulus.Patch (sB, `Remove xB));
        React.Step.execute step;
        loop (n_it - 1) sA sB
     | 6 ->
        let xA, xB = Random.int n, Random.int n in
        let sA, sB = Int_set.remove xA sA, Int_set.add xB sB in
        let step = React.Step.create () in
        send_csA ~step (Cumulus.Patch (sA, `Remove xA));
        send_csB ~step (Cumulus.Patch (sB, `Add xB));
        React.Step.execute step;
        loop (n_it - 1) sA sB
     | 7 ->
        let xA, xB = Random.int n, Random.int n in
        let sA, sB = Int_set.remove xA sA, Int_set.remove xB sB in
        let step = React.Step.create () in
        send_csA ~step (Cumulus.Patch (sA, `Remove xA));
        send_csB ~step (Cumulus.Patch (sB, `Remove xB));
        React.Step.execute step;
        loop (n_it - 1) sA sB
     | _ -> assert false)
  end in
  loop n Int_set.empty Int_set.empty

let () = test 5000
