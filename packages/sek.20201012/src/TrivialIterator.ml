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

open PublicTypeAbbreviations
open PrivateSignatures

module[@inline] Make (S : sig
  type 'a t
  val length : 'a t -> length
  val get : 'a t -> index -> 'a
  val get_segment : pov -> 'a t -> index -> 'a segment
end)
= struct

  type 'a iter = {
    (* The sequence over which we are iterating. *)
    s: 'a S.t;
    (* The length of the sequence. This is redundant;
       we cache it here for performance. *)
    n: length;
    (* Our current index, comprised between -1 and [n], both included. *)
    mutable i: index;
  }

  let check { n; i; _ } =
    assert (0 <= n);
    assert (-1 <= i && i <= n)

  let[@specialise] create pov s =
    let n = S.length s in
    let i = match pov with Front -> 0 | Back -> n - 1 in
    { s; n; i }

  let[@specialise] reset pov ({ n; _ } as it) =
    it.i <- match pov with Front -> 0 | Back -> n - 1

  let[@inline] copy { s; n; i } =
    { s; n; i }

  let[@inline] sequence { s; _ } =
    s

  let[@inline] length { n; _ } =
    n

  let[@inline] index { i; _ } =
    i

  let[@inline] finished { n; i; _ } =
    i = -1 || i = n

  let[@inline] get ({ s; i; _ } as it) =
    if finished it then
      raise End
    else
      S.get s i

  let set _it =
    (* We could implement support for [set] here, but we don't need
       it because this functor is used for persistent sequences only. *)
    assert false

  let is_valid _it =
    true

  let[@inline] get_segment pov ({ s; i; _ } as it) =
    if finished it then
      raise End
    else
      S.get_segment pov s i

  let get_writable_segment _pov _it =
    (* We could implement [get_writable_segment], but don't need it because
       this functor is used for persistent sequences only. *)
    assert false

  let[@inline] reach ({ n; _ } as it) j =
    assert (-1 <= j && j <= n);
    it.i <- j

  let[@specialise] jump pov ({ n; i; _ } as it) k =
    match pov with
    | Front ->
        if i + k <= n then
          it.i <- i + k
        else
          invalid_arg "move: attempt to move beyond the sentinel"
    | Back ->
        if -1 <= i - k then
          it.i <- i - k
        else
          invalid_arg "move: attempt to move beyond the sentinel"

  let[@inline] move pov it =
    jump pov it 1

  let print _element { i; _ } =
    PPrint.utf8format "TrivialIterator { i = %d }" i

end (* Make *)
