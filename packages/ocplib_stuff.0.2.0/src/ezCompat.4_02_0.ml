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

module Bytes = Bytes
module Buffer = Buffer

module String = struct
  include String
  let set = Bytes.set

  (* added in 4.03 *)
  let lowercase_ascii = lowercase
  let uppercase_ascii = uppercase
  let capitalize_ascii = capitalize
end

module Char = struct
  include Char

  (* added in 4.03 *)
  let uppercase_ascii = uppercase
  let lowercase_ascii = lowercase
end

module StringSet = struct
  module M = Set.Make(String)
  include M

  let of_list list =
    let map = ref empty in
    List.iter (fun x -> map := add x !map) list;
    !map

  let to_list set =
    let list = ref [] in
    iter (fun e -> list := e :: !list) set;
    List.rev !list

end

module StringMap = struct
  module M = Map.Make(String)
  include M
  let of_list list =
    let map = ref empty in
    List.iter (fun (x,y) -> map := add x y !map) list;
    !map

  let to_list map =
    let list = ref [] in
    iter (fun x y -> list := (x,y) :: !list) map;
    List.rev !list

  let to_list_of_keys map =
    let list = ref [] in
    iter (fun x y -> list := x :: !list) map;
    List.rev !list
end

module IntSet : sig

  include Set.S with type elt = int

end = struct

  module Set = Set.Make(struct type t = int
                               let compare (x:int) y = compare x y
                        end)

  include Set
end

module IntMap : sig

  include Map.S with type key = int

  val to_list: 'a t -> (int * 'a) list
  val to_list1: 'a t -> int list
  val to_list2: 'a t -> 'a list

  exception MinElt
  val min_elt: 'a t -> (key * 'a) option

end = struct
  module Map = Map.Make(struct type t = int
                               let compare (x:int) y = compare x y
                        end)

  include Map

  let to_list map =
    let list = ref [] in
    iter (fun x y -> list := (x,y) :: !list) map;
    List.rev !list

  let to_list1 map =
    let list = ref [] in
    iter (fun x _y -> list := x :: !list) map;
    List.rev !list

  let to_list2 map =
    let list = ref [] in
    iter (fun _x y -> list := y :: !list) map;
    List.rev !list

  exception MinElt
  let exn_MinElt = MinElt

  let min_elt map =
    let x = ref None in
    try
      iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
      None
    with MinElt -> !x

end
