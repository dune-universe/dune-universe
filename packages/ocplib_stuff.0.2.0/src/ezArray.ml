(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let rev t =
  let len = Array.length t in
  let last = len-1 in
  for i = 0 to len/2 - 1 do
    let x = t.(i) in
    t.(i) <- t.(last-i);
    t.(last-i) <- x;
  done

let () =
  for i = 0 to 5 do
    let t = Array.init i (fun i -> i) in
    let rev_t = Array.of_list (List.rev (Array.to_list t)) in
    rev t;
    assert (t = rev_t)
  done
