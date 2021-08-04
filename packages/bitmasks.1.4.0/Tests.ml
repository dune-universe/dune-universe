(* ********************************************************************************************** *
 * MetaStack Solutions Ltd.                                                                       *
 * ********************************************************************************************** *
 * BitMask Test Suite                                                                             *
 * ********************************************************************************************** *
 * Copyright (c) 2017 MetaStack Solutions Ltd.                                                    *
 * ********************************************************************************************** *
 * Author: David Allsopp                                                                          *
 * 13-Dec-2017                                                                                    *
 * ********************************************************************************************** *
 * Redistribution and use in source and binary forms, with or without modification, are permitted *
 * provided that the following two conditions are met:                                            *
 *     1. Redistributions of source code must retain the above copyright notice, this list of     *
 *        conditions and the following disclaimer.                                                *
 *     2. Neither the name of MetaStack Solutions Ltd. nor the names of its contributors may be   *
 *        used to endorse or promote products derived from this software without specific prior   *
 *        written permission.                                                                     *
 *                                                                                                *
 * This software is provided by the Copyright Holder 'as is' and any express or implied           *
 * warranties, including, but not limited to, the implied warranties of merchantability and       *
 * fitness for a particular purpose are disclaimed. In no event shall the Copyright Holder be     *
 * liable for any direct, indirect, incidental, special, exemplary, or consequential damages      *
 * (including, but not limited to, procurement of substitute goods or services; loss of use,      *
 * data, or profits; or business interruption) however caused and on any theory of liability,     *
 * whether in contract, strict liability, or tort (including negligence or otherwise) arising in  *
 * any way out of the use of this software, even if advised of the possibility of such damage.    *
 * ********************************************************************************************** *)

type t = A | B | C | D | E

module type T =
  sig
    type elt = t

    include BitMaskSet.S with type elt := elt
                         with type storage = int64
                         with type t = private int64
  end

module BMBasic : T =
  struct
    type elt = t

    module BM =
      struct
        type t = elt

        let mask = 0b11111L

        include BitMaskSet.Int64
      end

    include BitMaskSet.Make(BM)
  end

module BMEvil : T =
  struct
    type elt = t

    module BM =
      struct
        type t = elt

        let mask = 0b1100000100110L

        include BitMaskSet.Int64
      end

    include BitMaskSet.Make(BM)
  end

module BMDevil : T =
  struct
    type elt = t

    module BM =
      struct
        type t = elt

        let mask = 0b1100000000000000000000000000000000000000000000000000000000100110L

        include BitMaskSet.Int64
      end

    include BitMaskSet.Make(BM)
  end

let verify msg test =
  let outcome =
    try
      if test () then "PASSED" else "FAIL"
    with e ->
      "EXCEPTION " ^ Printexc.to_string e
  in
  Printf.printf "%s: %s\n%!" msg outcome

let verify_exn exn msg test =
  let outcome =
    try
      let _ = test () in
      "FAIL"
    with e ->
      if e = exn then
        "PASSED"
      else
        "EXCEPTION " ^ Printexc.to_string e
  in
  Printf.printf "%s: %s\n%!" msg outcome

module Make(T : T) =
  struct
    let test msg =
      Printf.printf "Testing %s...\n" msg;
      let set = T.add A T.empty in
      (fun () -> T.add A set == set) |> verify "Physical equality on add";
      (fun () -> T.remove B set == set) |> verify "Physical equality on remove";
      (fun () -> T.map (function x -> x) set == set) |> verify "Physical equality on map";
      (fun () -> T.map (function x -> x) T.empty == T.empty)
        |> verify "Physical equality on mapping empty set";
      let set_a_c = T.add C set in
      let set_a_b = T.add B set in
      (fun () -> T.filter_map (function C -> Some B | x -> Some x) set_a_c = set_a_b)
        |> verify "filter_map";
      (fun () -> T.filter_map (fun x -> Some x) set == set)
        |> verify "Physical equality on filter_map";
      let set = T.add E set in
      (fun () -> T.elements set = [A; E]) |> verify "elements";
      (fun () -> T.cardinal set = 2) |> verify "cardinal";
      (fun () -> T.min_elt T.empty = A)
        |> verify_exn Not_found "min_elt raises Not_found for empty";
      (fun () -> T.min_elt_opt T.empty = None) |> verify "min_elt_opt returns None for empty";
      (fun () -> T.max_elt T.empty = A)
        |> verify_exn Not_found "max_elt raises Not_found for empty";
      (fun () -> T.max_elt_opt T.empty = None) |> verify "max_elt_opt returns None for empty";
      (fun () -> T.min_elt set = A) |> verify "min_elt lowest element";
      (fun () -> T.min_elt (T.add C T.empty) = C) |> verify "min_elt non-lowest element";
      (fun () -> T.min_elt_opt set = Some A) |> verify "min_elt_opt";
      (fun () -> T.max_elt set = E) |> verify "max_elt";
      (fun () -> T.max_elt_opt set = Some E) |> verify "max_elt_opt";
      (fun () -> T.find_opt C set = None) |> verify "find_elt_opt returns None for Not_found";
      (fun () -> T.find_opt A set = Some A) |> verify "find_elt_opt";
      let set = T.add D (T.add C T.empty) in
      (fun () -> T.fold (fun _ -> succ) set 0 = 2) |> verify "fold";
      (fun () -> (let x = ref 0 in T.iter (fun _ -> incr x) set; !x) = 2) |> verify "iter";
      let f = function D | E -> false | _ -> true in
      (fun () -> T.find_first f set = C) |> verify "find_first";
      (fun () -> T.find_last f set = C) |> verify "find_last";
      (fun () -> T.find_first_opt f set = Some C) |> verify "find_first_opt";
      (fun () -> T.find_last_opt f set = Some C) |> verify "find_last_opt";
      let set = T.add D (T.add E T.empty) in
      (fun () -> T.find_first_opt f set = None)
        |> verify "find_first_opt returns None for Not_found";
      (fun () -> T.find_last_opt f set = None) |> verify "find_last_opt returns None for Not_found";
      let set_c = T.add C T.empty in
      let set_e = T.add E T.empty in
      let set_c_e = T.add C set_e in
      let set = T.add A set_c_e in
      let is_e = function E -> true | _ -> false in
      let is_not_e = function E -> true | _ -> false in
      (fun () -> T.split C set = (T.add A T.empty, true, set_e)) |> verify "split";
      (fun () -> T.exists is_e set) |> verify "exists";
      (fun () -> T.for_all is_not_e set = false) |> verify "for_all";
      (fun () -> T.filter is_e set = set_e) |> verify "filter";
      (fun () -> T.partition is_e set = (set_e, T.add A set_c)) |> verify "partition";
      (fun () -> T.equal (T.of_seq (T.to_seq set)) set) |> verify "of_seq / to_seq";
      (fun () -> T.equal (T.add_seq (Seq.return C) set_e) set_c_e) |> verify "add_seq";
      (fun () -> T.to_seq_from C set |> Seq.fold_left (fun a e -> e::a) [] = [E; C])
        |> verify "to_seq_from";
      (fun () -> T.to_rev_seq set |> Seq.fold_left (fun a e -> e::a) [] = [A; C; E])
        |> verify "to_rev_seq"
  end

module Basic = Make(BMBasic)
module Evil = Make(BMEvil)
module Devil = Make(BMDevil)

let () = Basic.test "Basic bitmask"
let () = Evil.test "Evil bitmask"
let () = Devil.test "Devilish bitmask"
