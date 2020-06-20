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

open PublicSignature

(* This functor allows providing a [default] element up front, once and for
   all, thereby removing the need from passing it as an argument to [create],
   [of_array], etc. *)

(* Although this code is reasonably short, the signature of this functor is
   quite long, as it is a variant of the signature [SEK]. For this reason,
   we do not provide an [.mli] file. *)

(* The repetition between the ephemeral variant and the persistent variant
   could be eliminated by introducing a helper functor. However, tolerating
   this repetition leads to more readable code. *)

module[@inline] SupplyDefault
  (S : SEK)
  (D : sig type element val default: element end)
= struct

  (* In OCaml < 4.08, we cannot include [S] here.
     We list its toplevel components. *)
  type side = S.side
  let front = S.front
  let back = S.back
  let other = S.other
  type direction = S.direction
  let forward = S.forward
  let backward = S.backward
  let opposite = S.opposite
  let sign = S.sign
  exception Empty = S.Empty
  exception End = S.End
  let snapshot = S.snapshot
  let snapshot_and_clear = S.snapshot_and_clear
  let edit = S.edit
  let released = S.released

  module Ephemeral = struct
    include S.Ephemeral
    open D
    (* In OCaml < 4.08, we cannot redefine the type [t]. *)
    (* type t = element E.t *)
    let[@inline] create () = create default
    let[@inline] make n v = make default n v
    let[@inline] init n f = init default n f
    let[@inline] of_list_segment n xs = of_list_segment default n xs
    let[@inline] of_list xs = of_list default xs
    let[@inline] of_array_segment a i k = of_array_segment default a i k
    let[@inline] of_array a = of_array default a
    let[@inline] of_seq_segment n xs = of_seq_segment default n xs
    let[@inline] of_seq xs = of_seq default xs
  end (* E *)

  module Persistent = struct
    include S.Persistent
    open D
    (* In OCaml < 4.08, we cannot redefine the type [t]. *)
    (* type t = element P.t *)
    let[@inline] create () = create default
    let empty = create()
    let[@inline] make n v = make default n v
    let[@inline] init n f = init default n f
    let[@inline] of_list_segment n xs = of_list_segment default n xs
    let[@inline] of_list xs = of_list default xs
    let[@inline] of_array_segment a i k = of_array_segment default a i k
    let[@inline] of_array a = of_array default a
    let[@inline] of_seq_segment n xs = of_seq_segment default n xs
    let[@inline] of_seq xs = of_seq default xs
    let[@inline] map f s = map default f s
    let[@inline] mapi f s = mapi default f s
    let[@inline] filter_map f s = filter_map default f s
    let[@inline] flatten_map f s = flatten_map default f s
    let[@inline] map2 f s1 s2 = map2 default f s1 s2
  end (* P *)

  module E = Ephemeral
  module P = Persistent

  module Emulated = struct

    module Array = struct
      include S.Emulated.Array
      open D
      let[@inline] make n x = make default n x
      let[@inline] init n f = init default n f
      let[@inline] make_matrix m n x = make_matrix default m n x
      let[@inline] concat ss = concat default ss
      let[@inline] of_list xs = of_list default xs
      let[@inline] map f s = map default f s
      let[@inline] mapi f s = mapi default f s
      let[@inline] map2 f s1 s2 = map2 default f s1 s2
      let[@inline] of_seq s = of_seq default s
    end

    module List = struct
      include S.Emulated.List
      open D
      let empty = P.empty (* We cannot name it [[]]. *)
      let[@inline] init n f = init default n f
      let[@inline] map f s = map default f s
      let[@inline] mapi f s = mapi default f s
      let[@inline] rev_map f s = rev_map default f s
      let[@inline] filter_map f s = filter_map default f s
      let[@inline] concat_map f s = concat_map default f s
      let[@inline] map2 f s1 s2 = map2 default f s1 s2
      let[@inline] rev_map2 f s1 s2 = rev_map2 default f s1 s2
      let[@inline] fold_left_map f accu s = fold_left_map default f accu s
      let[@inline] of_seq s = of_seq default s
    end (* List *)

    module Queue = struct
      include S.Emulated.Queue
      open D
      let[@inline] create () = create default
      let[@inline] of_seq xs = of_seq default xs
    end (* Queue *)

    module Stack = struct
      include S.Emulated.Stack
      open D
      let[@inline] create () = create default
      let[@inline] of_seq xs = of_seq default xs
    end (* Stack *)

  end (* Emulated *)

end (* SupplyDefault *)
