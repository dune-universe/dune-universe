(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  type mass

  type 'a t

  type 'a state

  val create : ('a * mass) list -> 'a state

  val sampler : 'a state -> 'a t
end

module Make
    (Mass : Basic_intf.Ring)
    (Mass_ord : Basic_intf.Infix_order with type 'a m := 'a and type t = Mass.t)
    (M : Basic_intf.Monad) (Sampler : sig
      type 'a t = 'a M.t

      val mass : Mass.t -> Mass.t t

      val int : int -> int t
    end) : S with type mass = Mass.t and type 'a t = 'a M.t = struct
  type mass = Mass.t

  type 'a t = 'a M.t

  type 'a state =
    { total : Mass.t; support : 'a array; p : Mass.t array; alias : int array }

  let rec init_loop total p alias small large =
    match (small, large) with
    | ([], _) -> List.iter (fun (_, i) -> Array.set p i total) large
    | (_, []) ->
        (* This can only happen because of numerical inaccuracies when using
           eg [Mass.t = float] *)
        List.iter (fun (_, i) -> Array.set p i total) small
    | ((qi, i) :: small', (qj, j) :: large') ->
        Array.set p i qi ;
        Array.set alias i j ;
        let qj' = Mass.sub (Mass.add qi qj) total in
        if Mass_ord.(qj' < total) then
          init_loop total p alias ((qj', j) :: small') large'
        else init_loop total p alias small' ((qj', j) :: large')

  let support : fallback:'a -> length:int -> ('a * Mass.t) list -> 'a Array.t =
   fun ~fallback ~length measure ->
    let a = Array.make length fallback in
    List.iteri (fun i (elt, _) -> Array.set a i elt) measure ;
    a

  let check_and_cleanup measure =
    let (total, measure) =
      List.fold_left
        (fun ((total, m) as acc) ((_, p) as point) ->
          if Mass_ord.(Mass.zero < p) then (Mass.add total p, point :: m)
          else if Mass_ord.(p < Mass.zero) then invalid_arg "create"
          else (* p = zero: drop point *)
            acc)
        (Mass.zero, [])
        measure
    in
    match measure with
    | [] -> invalid_arg "create"
    | (fallback, _) :: _ -> (fallback, total, measure)

  (* NB: duplicate elements in the support are not merged;
     the algorithm should still function correctly. *)
  let create (measure : ('a * Mass.t) list) =
    let (fallback, total, measure) = check_and_cleanup measure in
    let length = List.length measure in
    let n = Mass.of_int length in
    let (_, small, large) =
      List.fold_left
        (fun (i, small, large) (_, p) ->
          let q = Mass.mul p n in
          if Mass_ord.(q < total) then (i + 1, (q, i) :: small, large)
          else (i + 1, small, (q, i) :: large))
        (0, [], [])
        measure
    in
    let support = support ~fallback ~length measure in
    let p = Array.make length total in
    let alias = Array.make length (-1) in
    init_loop total p alias small large ;
    { total; support; p; alias }

  let sampler { total; support; p; alias } =
    let open M.Infix in
    let n = Array.length support in
    Sampler.int n >>= fun i ->
    let p = Array.get p i in
    Sampler.mass total >>= fun elt ->
    if Mass_ord.(elt < p) then M.return (Array.get support i)
    else
      let j = Array.get alias i in
      assert (j >= 0) ;
      M.return (Array.get support j)
end
