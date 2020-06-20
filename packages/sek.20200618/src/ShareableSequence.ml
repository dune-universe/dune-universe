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
open PublicSettings
open PrivateSignatures

module[@inline] Make
  (SChunk : SCHUNK)
  (C : CAPACITY)
= struct
open C

type 'a schunk =
  'a SChunk.t

type 'a measure = 'a SChunk.measure =
| MUnit         : 'a measure
| MSWeight : 'a schunk measure

let apply =
  SChunk.apply

(* -------------------------------------------------------------------------- *)

(* A shareable chunk sequence is organized in several levels. It can be
   thought of as a list of levels, where the constructors [Zero] and [Level]
   play the roles of nil and cons. *)

(* The number of levels is logarithmic in the number of elements in the
   sequence. *)

(* The data constructor [Zero] represents an empty sequence. It carries a
   [default] element, which is used when creating an empty chunk. *)

(* A chunk at depth [depth] has capacity [capacity depth]. *)

(* The [Level] constructor represents a nonempty sequence. It carries the
   total [weight] of the elements of the sequence, followed with a [front]
   schunk, which contains elements, a [middle] sequence, whose elements are
   *schunks* of elements, and a [back] schunk, which contains elements. *)

(* We impose the following "populated-sides invariant": if the front or back
   schunk is empty, then the middle sequence must be empty as well. In the
   contrapositive, if the middle sequence is nonempty, then the front and back
   schunks must be nonempty. *)

(* We impose the following "density invariant": in the middle sequence, the
   sum of the lengths of two adjacent schunks must exceed [capacity depth]. We
   require that this property also hold of the middle sequence in front of
   which a fictitious full schunk has been prepended. As a result, the density
   invariant implies that no schunk in the middle sequence is empty. *)

(* The constructors [One] and [Short] play no role in this file. They are
   added in anticipation of the code in [ShortPersistentSequence], which deals
   with a more compact representation of short persistent sequences.
   Therefore, many of the functions in this file do not handle these cases.
   Some functions do handle these cases, because it is easy to do so here (and
   this saves a test elsewhere). These functions are: [default], [dummy],
   [weight], [is_empty], [iter_segments], [to_array], [print], [peek]. *)

type 'a t =
  | Zero  of { default : 'a; }
  | One   of { default : 'a; x : 'a }
  | Short of { default : 'a; a : 'a array }
  | Level of {
      weight : weight;
      front : 'a schunk;
      middle : 'a schunk t;
      back : 'a schunk;
    }

(* The structure of a [Level] is symmetric: front and back can be exchanged,
   yielding the same structure. This allows us to share code by parameterizing
   certain operations with a point of view. *)

(* We use the words [this] and [that] to refer to this side -- the one closest
   to us, from our point of view -- and that side -- the other side. *)

(* -------------------------------------------------------------------------- *)

(* Basic accessors. *)

let[@inline] default s =
  match s with
  | Zero  { default; _ }
  | One   { default; _ }
  | Short { default; _ } ->
      default
  | Level { front; _ } ->
      SChunk.default front

(* [dummy s] extracts a dummy schunk out of [s], if possible; otherwise, it
   creates a fresh one. *)

let[@inline] dummy s =
  match s with
  | Zero  { default; _ }
  | One   { default; _ }
  | Short { default; _ } ->
      SChunk.dummy default
  | Level { middle; _ } ->
      (* The default element of the middle sequence is a dummy schunk. *)
      default middle

(* [weight_of] is so named to avoid a clash with the field name [weight].
   It is ultimately exported under the name [weight]. *)

let[@inline] weight_of s =
  match s with
  | Zero _ ->
      0
  | One _ ->
      1
  | Short { a; _ } ->
      Array.length a
  | Level { weight; _ } ->
      weight

let[@inline] is_empty s =
  match s with
  | Zero _ ->
      true
  | One _
  | Short _
  | Level _ ->
      false

(* -------------------------------------------------------------------------- *)

(* Construction and destruction. *)

let create default =
  Zero { default }

let[@inline] create_middle default =
  let default = SChunk.dummy default in
  create default

(* [nonempty_level] is a constructor, parameterized by a point of view. As we
   do not allow the constructor [Level] to represent an empty sequence, this
   constructor cannot be used in the case where the sequence is empty. *)

let[@specialise] nonempty_level pov weight this middle that =
  assert (weight > 0);
  let front, back = exchange pov this that in
  Level { weight; front; middle; back }

(* [level] is also a constructor, parameterized by a point of view. It
   does not require the sequence to be nonempty. It does not guarantee
   that the populated-sides invariant holds. *)

let[@inline] level pov weight this middle that =
  if weight = 0 then
    let default = SChunk.default this in
    create default
  else
    nonempty_level pov weight this middle that

(* [promote p o] converts the schunk [p] to a sequence. *)

let promote p o =
  let default = SChunk.default p in
  let weight = SChunk.weight p in
  (* An empty front schunk. *)
  let front = SChunk.create default (SChunk.capacity p) o in
  (* An empty middle sequence. *)
  let middle = create_middle default in
  (* The schunk [p] becomes the back schunk. *)
  let back = p
    (* or: [SChunk.copy p o], but that would be more expensive *)
  in
  level Front weight front middle back

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let[@specialise] rec iter_segments
: 'a. pov -> 'a t -> 'a segments
= fun pov s yield ->
  match s with
  | Zero _ ->
      ()
  | One { x; _ } ->
      yield ([|x|], 0, 1)
  | Short { a; _ } ->
      yield (a, 0, Array.length a)
  | Level { front; middle; back; _ } ->
      let this, that = exchange pov front back in
      SChunk.iter_segments pov this yield;
      iter pov (fun p -> SChunk.iter_segments pov p yield) middle;
      SChunk.iter_segments pov that yield

and[@specialise] iter
: 'a. pov -> ('a -> unit) -> 'a t -> unit
= fun pov f s ->
  ArrayExtra.iter iter_segments pov f s

let iter_left g s =
  iter Front g s

let iter_right g s =
  iter Back g s

let fold_left f seed s =
  Adapters.fold_left iter_left f seed s

let to_list s =
  Adapters.to_list iter_right s

(* -------------------------------------------------------------------------- *)

(* Conversion to an array. *)

(* We assume zero depth and unit weight. Thus, the weight of the sequence
   is also its length. *)

(* The code in the last case, [Level], would in fact work in all cases,
   but we can save a little time by dealing explicitly with each case. *)

let to_array s =
  match s with
  | Zero _ ->
      [||]
  | One { x; _ } ->
      [|x|]
  | Short { a; _ } ->
      Array.copy a
  | Level _ ->
      ArrayExtra.concat_segments Front (default s) (weight_of s)
        (iter_segments Front s)

(* -------------------------------------------------------------------------- *)

(* Printing. *)

(* The [model] pseudo-field is printed only when [m] is [MUnit],
   otherwise this creates too much noise. *)

module Printing = struct

  open PPrint
  open PPrint.OCaml

  let rec print : 'a. 'a measure -> ('a -> document) -> 'a t -> document =
    fun m element s ->
    match s with
    | Zero _ ->
        !^ "Zero"
    | One { x; _ } ->
        !^ "One" ^^ record "pwseq" [
          "model", flowing_list element [x]
        ]
    | Short { a; _ } ->
        !^ "Short" ^^ record "pwseq" [
          "model", flowing_list element (Array.to_list a)
        ]
    | Level { front; middle; back; weight } ->
        let schunk = SChunk.print m element in
        !^ "Level " ^^ record "pwseq" ([
          "weight", int weight;
          "front", schunk front;
          "middle", print MSWeight schunk middle;
          "back", schunk back;
        ] @ (if m = MUnit then [
          "model", flowing_list element (to_list s);
        ] else []))

end

include Printing

(* -------------------------------------------------------------------------- *)

(* Validation. *)

let rec check : 'a. 'a t -> 'a measure -> owner -> depth -> unit =
  fun s m o depth ->
  match s with
  | Zero _ ->
      ()
  | One _
  | Short _ ->
      (* The data constructors [One] and [Short] are not allowed to appear
         under [Level]. Furthermore, [check] must not be applied to a
         sequence that has of these data constructors at the root. *)
      assert false
  | Level { weight; front; middle; back } ->

      (* Since the sequence is nonempty, its weight must be positive. *)
      assert (weight > 0);
      (* Check the populated-sides invariant. *)
      if SChunk.is_empty front || SChunk.is_empty back then
        assert (is_empty middle);
      (* Check that the front and back schunks are well-formed. *)
      SChunk.check m o front;
      SChunk.check m o back;
      (* Check the middle sequence (before iterating on it). *)
      check_middle middle m o depth;
      (* Check that the weight is correct. *)
      assert (
        weight =
          SChunk.weight front +
          fold_left (fun sum p -> sum + SChunk.weight p) 0 middle +
          SChunk.weight back
      )

and check_middle : 'a. 'a schunk t -> 'a measure -> owner -> depth -> unit =
  fun middle m o depth ->
  (* Check that the sequence is well-formed (before iterating on it). *)
  check middle MSWeight o (depth + 1);
  (* Check that every element in the middle sequence is a well-formed
     schunk. Note that this is *not* a consequence of the fact that
     the middle sequence is well-formed. *)
  iter Front (SChunk.check m o) middle;
  (* Check the density invariant: in the middle sequence, the sum of the
     lengths of any two adjacent schunks must exceed [capacity depth]. *)
  let n = capacity depth in
  let previous_length = ref n in (* a fictitious full schunk *)
  iter Front (fun p ->
    assert (!previous_length + SChunk.length p > n);
    previous_length := SChunk.length p
  ) middle

(* Ensure [check] has zero cost in release mode. *)

let[@inline] check s m o depth =
  assert (check s m o depth; true)

(* -------------------------------------------------------------------------- *)

(* Peek. *)

let[@specialise] peek pov s =
  match s with
  | Zero _ ->
      raise Empty
  | One { x; _ } ->
      x
  | Short { a; _ } ->
      assert (2 <= Array.length a);
      begin match pov with
      | Front ->
          Array.get a 0
      | Back ->
          let n = Array.length a in
          Array.get a (n-1)
      end
  | Level { front; back; _ } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = exchange pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      if not (SChunk.is_empty this) then
        SChunk.peek pov this
      else
        SChunk.peek pov that

(* -------------------------------------------------------------------------- *)

(* Push. *)

let[@specialise] rec push
: 'a. pov -> 'a t -> 'a -> 'a measure -> owner -> depth -> 'a t
= fun pov s x m o depth ->
  match s with
  | Zero { default } ->

      (* Create an empty schunk. *)
      let p = SChunk.create default (capacity depth) o in
      (* Push [x] into it. *)
      let p = SChunk.push pov p x m o in
      (* Promote it to a sequence. *)
      promote p o

  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false

  | Level { weight; front; middle; back } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = exchange pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      let weight = weight + apply m x in
      if not (SChunk.is_full this) then begin
        (* There is room in the front schunk. Push [x] into it. *)
        let this = SChunk.push pov this x m o in
        nonempty_level pov weight this middle that
      end
      else if SChunk.is_empty that then begin
        (* The front schunk is full. *)
        (* The back schunk is empty. Per the populated-sides invariant,
           the middle sequence must be empty as well. *)
        assert (is_empty middle);
        (* Push [x] into the back schunk and exchange the front and back
           schunks, so [x] ends up in front, as desired. *)
        let this, that = SChunk.push pov that x m o, this in
        nonempty_level pov weight this middle that
      end
      else begin
        (* The front schunk is full. The middle sequence and back schunk
           are nonempty. *)
        (* Create an empty schunk, push [x] into it, and make it the new
           front chunk, while pushing the previous front chunk into the
           [middle] sequence. *)
        let p = SChunk.create (SChunk.default this) (capacity depth) o in
        let this, middle =
          SChunk.push pov p x m o,
          push pov middle this MSWeight o (depth + 1)
        in
        nonempty_level pov weight this middle that
      end

(* -------------------------------------------------------------------------- *)

(* Pop. *)

let[@specialise] rec pop : 'a. pov -> 'a t -> 'a measure -> owner -> 'a * 'a t =
  fun pov s m o ->
  match s with
  | Zero _ ->

      raise Empty

  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false

  | Level { weight; front; middle; back } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = exchange pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      if SChunk.is_empty this then begin
        (* The front schunk is empty. Per the populated-sides invariant,
           the middle sequence must be empty as well. Thus, the back
           schunk cannot be empty. *)
        assert (is_empty middle);
        assert (not (SChunk.is_empty that));
        (* Pop an element [x] off the back schunk. *)
        let x, that = SChunk.pop pov that m o in
        let weight = weight - apply m x in
        x, level pov weight this middle that
      end
      else begin
        (* Pop an element [x] off the front schunk. *)
        let x, this = SChunk.pop pov this m o in
        let weight = weight - apply m x in
        (* This test is identical to the one in [level]. *)
        if weight = 0 then
          let default = SChunk.default this in
          x, create default
        else
          (* Restore the populated-sides invariant. *)
          (* This is a specialized version of [populate]. *)
          let this, middle =
            if SChunk.is_empty this && not (is_empty middle)
            then pop pov middle MSWeight o
            else this, middle
          in
          x, nonempty_level pov weight this middle that
      end

(* -------------------------------------------------------------------------- *)

(* Restoring the populated-sides invariant. *)

(* These functions use [pop], which is why they are placed here. *)

(* [populate pov s o] restores one side of the populated-sides invariant. *)

let[@inline] populate pov s o =
  match s with
  | Zero _ ->
      s

  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false

  | Level { weight; front; middle; back } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = exchange pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      if SChunk.is_empty this && not (is_empty middle) then begin
        (* The invariant is broken on this side: the front chunk is
           empty, yet the middle sequence is nonempty. Restore the
           invariant by popping one element off the middle sequence
           and making it the front chunk. *)
        let this, middle = pop pov middle MSWeight o in
        assert (not (SChunk.is_empty this));
        nonempty_level pov weight this middle that
      end
      else
        s

(* The smart constructor [populate_both_nonempty_level] requires the sequence
   to be nonempty and restores the populated-sides invariant if necessary. It
   is not parameterized by a point of view. *)

let populate_both_nonempty_level weight front middle back o =
  assert (weight > 0);
  let s = Level { weight; front; middle; back } in
  let s = populate Front s o in
  let s = populate Back s o in
  s

(* The smart constructor [populate_nonempty_level] requires the sequence to be
   nonempty and restores one side of the populated-sides invariant. *)

let[@specialise] populate_nonempty_level pov weight front middle back o =
  assert (weight > 0);
  let s = Level { weight; front; middle; back } in
  let s = populate pov s o in
  s

(* The smart constructor [populate_level] supports the case where the sequence
   is empty and restores one side of the populated-sides invariant. *)

let[@inline] populate_level pov weight front middle back o =
  if weight = 0 then
    let default = SChunk.default front in
    create default
  else
    populate_nonempty_level pov weight front middle back o

(* -------------------------------------------------------------------------- *)

(* Constructors for sequences of a known size [size], operating only at depth
   zero, with unit weights. *)

(* [create_by_segments default size o create_schunk] builds a sequence
   of length [size]. It expects a function [create_schunk] such that
   [create_schunk n i k] produces a schunk of capacity [n] for the
   subsequence of length [k] that begins at index [i]. *)

let create_by_segments default size o create_schunk =
  (* We assume the depth is zero. *)
  let depth = 0 in
  (* A fast path for empty sequences. *)
  if size = 0 then
    create default
  else begin
    let n = capacity depth in
    (* We assume unit weight. *)
    let weight = size in
    (* This calling convention is more convenient here. *)
    let[@inline] create_schunk (i, k) = create_schunk n i k in
    (* Cut the range into a nonempty front segment, a possibly empty
       series of segments of size [n], and a possibly empty back segment.
       [cut] guarantees that if the back segment is empty then the middle
       sequence is empty as well. *)
    let front, foreach_middle_segment, back = ArrayExtra.cut n n size in
    (* The front, middle, and back must be created in this order. Indeed,
       [create_schunk] may have side effects, and the order of these side
       effects matters. *)
    (* Construct the front. *)
    let front = create_schunk front in
    (* Construct the middle. *)
    let middle = ref (create_middle default) in
    foreach_middle_segment (fun i k ->
      middle := push Back !middle (create_schunk (i, k)) MSWeight o (depth + 1)
    );
    let middle = !middle in
    (* Construct the back. *)
    let back = create_schunk back in
    (* Build a level. Because [cut] is designed to enforce the populated-sides
       invariant by construction, there is no need to call [populate]. *)
    Level { weight; front; middle; back }
  end

(* Conversion from an array. *)

let of_array_segment default a head size o =
  assert (Segment.is_valid (a, head, size));
  create_by_segments default size o (fun n i k ->
    (* Construct the segment [i, i + k) of the sequence using the elements in
       the segment [head + i, head + i + k) of the array [a]. *)
    SChunk.of_array_segment default n a (head + i) k o
  )

let make default size v o =
  assert (0 <= size);
  (* Because this is a persistent sequence, we can create a single schunk of
     capacity [n] and size [n], filled with the value [v], and re-use it over
     and over. *)
  (* This optimization relies on the assumption that [o] is [Owner.none]. *)
  assert (o = Owner.none);
  (* We apply this optimization only at depth 0, not at greater depths (even
     though it is possible in principle), because it is easy. *)
  let depth = 0 in
  let n = capacity depth in
  let p = SChunk.make default n n v o in
  create_by_segments default size o (fun n _i k ->
    assert (n = SChunk.capacity p);
    (* We need a schunk of capacity [n] and size [k], filled with [k] copies
       of the value [v]. *)
    (* If [k] is [n], we can use our ready-made schunk. If [k] is less than
       [n], we create a new schunk with a smaller view, based on the same
       support. *)
    if k = n then
      p
    else
      let p1, _x = SChunk.take p k MUnit o in
      p1
  )

let init default size f o =
  assert (0 <= size);
  create_by_segments default size o (fun n i k ->
    (* Allocate a schunk of capacity [n] and size [k],
       filled with the values [f i], [f (i+1)], ..., [f (i+k-1)]. *)
    SChunk.init default n k i f o
  )

(* -------------------------------------------------------------------------- *)

(* [fuse_back middle p o depth] pushes the schunk [p] onto the back of
   [middle], which is a sequence of schunks. If possible, the last schunk of
   the sequence and the schunk [p] are fused into a single schunk. This allows
   preserving the density invariant. *)

(* By convention, pass [depth], not [depth + 1] -- [fuse_back] takes care of
   adding one. *)

let[@inline] fuse_back middle p o depth =
  if SChunk.is_empty p then
    (* [p] is empty. There is nothing to do. *)
    middle
  else if is_empty middle then
    (* [middle] is empty. Just push [p] into [middle]. *)
    push Back middle p MSWeight o (depth + 1)
  else
    (* The capacity at the current depth is given by [SChunk.capacity p]. *)
    let n = SChunk.capacity p in
    if SChunk.length (peek Back middle) + SChunk.length p > n then
      (* Concatenating the last element of [middle] with [p] is forbidden.
         Just push [p] into [middle]. *)
      push Back middle p MSWeight o (depth + 1)
    else begin
      (* [middle] is nonempty and its last element [last] can be concatenated
         with [p]. Then, pop [last] off [middle], concatenate [last] and [p],
         and push the result back into [middle]. *)
      let last, middle = pop Back middle MSWeight o in
      let last = SChunk.concat last p o in
      push Back middle last MSWeight o (depth + 1)
    end

(* -------------------------------------------------------------------------- *)

(* [eject this middle that] returns either [this, that] or [that, this].
   It allows itself to exchange [this] and [that] only when both [this]
   and [middle] are empty, so the model of the sequence is not affected.
   Provided the sequence [this/middle/that] is nonempty, we are assured
   that the first component of the result is a nonempty schunk. *)

let[@inline] eject this middle that =
  if SChunk.is_empty this then begin
    assert (is_empty middle);
    that, this
  end
  else
    this, that

(* -------------------------------------------------------------------------- *)

(* Concatenation. *)

let rec concat : 'a. 'a t -> 'a t -> owner -> depth -> 'a t =
  fun s1 s2 o depth ->
  match s1, s2 with
  | Zero _, s
  | s, Zero _ ->
      s

  | (One _ | Short _), _
  | _, (One _ | Short _) ->
      (* Not handled here. *)
      assert false

  | Level { weight = weight1; front = front1; middle = middle1;
            back = back1 },
    Level { weight = weight2; front = front2; middle = middle2;
            back = back2 } ->
      (* Weight. *)
      let weight = weight1 + weight2 in
      (* Exchange the front and back schunks of [s1], if necessary, to ensure
         that its front schunk is nonempty. *)
      let front1, back1 = eject front1 middle1 back1 in
      assert (not (SChunk.is_empty front1));
      (* Similarly, ensure that the back schunk of [s2] is nonempty. *)
      let back2, front2 = eject back2 middle2 front2 in
      assert (not (SChunk.is_empty back2));
      (* Get rid of [back1] and [front2] by pushing them into [middle1]. *)
      let middle1 = fuse_back middle1 back1 o depth in
      let middle1 = fuse_back middle1 front2 o depth in
      (* There remains to concatenate the two middles, *)
      let middle = fuse middle1 middle2 o depth in
      (* and build a new level. *)
      populate_both_nonempty_level weight front1 middle back2 o

(* By convention, pass [depth], not [depth + 1] -- [fuse] takes care of adding
   one. *)

and fuse : 'a. 'a schunk t -> 'a schunk t -> owner -> depth -> 'a schunk t =
  fun s1 s2 o depth ->
    if is_empty s1 then
      s2
    else if is_empty s2 then
      s1
    else
      let last1 = peek Back s1
      and first2 = peek Front s2 in
      let n = SChunk.capacity last1 in
      assert (n = SChunk.capacity first2);
      if SChunk.length last1 + SChunk.length first2 > n then
        concat s1 s2 o (depth + 1)
      else
        let first2, s2 = pop Front s2 MSWeight o in
        let s1 = fuse_back s1 first2 o depth in
        concat s1 s2 o (depth + 1)

(* -------------------------------------------------------------------------- *)

(* Split. *)

let rec three_way_split :
  'a. 'a t -> weight -> 'a measure -> owner -> 'a t * 'a * 'a t
=
  fun s i m o ->
  match s with
  | Zero _ ->
      assert false

  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false

  | Level { weight; front; middle; back } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then begin

        (* The desired element lies in the front schunk. *)
        let front1, x, front2 = SChunk.three_way_split front i m o in
        let weight1 = SChunk.weight front1 in
        let s1 = promote front1 o in
        let weight2 = weight - weight1 - apply m x in
        let s2 = populate_level Front weight2 front2 middle back o in
        s1, x, s2

      end
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then begin
          let i = i - weight_middle in

          (* The desired element lies in the back schunk. *)
          let back1, x, back2 = SChunk.three_way_split back i m o in
          let weight2 = SChunk.weight back2 in
          let s2 = promote back2 o in
          let weight1 = weight - weight2 - apply m x in
          let s1 = populate_level Back weight1 front middle back1 o in
          s1, x, s2

        end
        else begin

          (* The desired element lies in the middle. *)
          (* Split the middle sequence. *)
          let middle1, p, middle2 = three_way_split middle i MSWeight o in
          (* We now know that the desired element lies in the schunk [p]. *)
          let weight_middle1 = weight_of middle1 in
          let i = i - weight_middle1 in
          (* Split this schunk! *)
          let p1, x, p2 = SChunk.three_way_split p i m o in
          (* To the left of [x], we have [front], [middle1], and [p1]. *)
          let weight1 =
            SChunk.weight front + weight_middle1 + SChunk.weight p1 in
          let s1 = populate_nonempty_level Back weight1 front middle1 p1 o in
          (* To the right of [x], we have [p2], [middle2], and [back]. *)
          let weight2 = weight - weight1 - apply m x in
            (* or: SChunk.weight p2 + weight_of middle2 + SChunk.weight back *)
          let s2 = populate_nonempty_level Front weight2 p2 middle2 back o in
          s1, x, s2

        end

(* Check that [three_way_split] satisfies its postcondition. *)

let[@inline] three_way_split s i m o =
  let result = three_way_split s i m o in
  assert (
    let s1, x, _s2 = result in
    weight_of s1 <= i && i < weight_of s1 + apply m x
  );
  result

(* -------------------------------------------------------------------------- *)

(* Take. *)

(* [take] is a specialized version of [three_way_split]. Instead of returning
   a triple [s1, x, s2], it returns only the pair [s1, x]. *)

let rec take :
  'a. 'a t -> weight -> 'a measure -> owner -> 'a t * 'a
=
  fun s i m o ->
  match s with
  | Zero _ ->
      assert false

  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false

  | Level { weight; front; middle; back } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then begin

        (* The desired element lies in the front schunk. *)
        let front1, x = SChunk.take front i m o in
        let s1 = promote front1 o in
        s1, x

      end
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then begin
          let i = i - weight_middle in

          (* The desired element lies in the back schunk. *)
          let back1, x = SChunk.take back i m o in
          let weight1 =
            SChunk.weight front + weight_of middle + SChunk.weight back1 in
          let s1 = populate_level Back weight1 front middle back1 o in
          s1, x

        end
        else begin

          (* The desired element lies in the middle. *)
          (* Split the middle sequence. *)
          let middle1, p = take middle i MSWeight o in
          (* We now know that the desired element lies in the schunk [p]. *)
          let weight_middle1 = weight_of middle1 in
          let i = i - weight_middle1 in
          (* Split this schunk! *)
          let p1, x = SChunk.take p i m o in
          (* To the left of [x], we have [front], [middle1], and [p1]. *)
          let weight1 =
            SChunk.weight front + weight_middle1 + SChunk.weight p1 in
          let s1 = populate_nonempty_level Back weight1 front middle1 p1 o in
          s1, x

        end

(* -------------------------------------------------------------------------- *)

(* Drop. *)

(* [drop] is a specialized version of [three_way_split]. Instead of returning
   a triple [s1, x, s2], it returns only the pair [x, s2]. *)

let rec drop :
  'a. 'a t -> weight -> 'a measure -> owner -> 'a * 'a t
=
  fun s i m o ->
  match s with
  | Zero _ ->
      assert false

  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false

  | Level { weight; front; middle; back } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then begin

        (* The desired element lies in the front schunk. *)
        let x, front2 = SChunk.drop front i m o in
        let weight2 =
          SChunk.weight front2 + weight_of middle + SChunk.weight back in
        let s2 = populate_level Front weight2 front2 middle back o in
        x, s2

      end
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then begin
          let i = i - weight_middle in

          (* The desired element lies in the back schunk. *)
          let x, back2 = SChunk.drop back i m o in
          let s2 = promote back2 o in
          x, s2

        end
        else begin

          (* The desired element lies in the middle. *)
          (* Split the middle sequence. *)
          let p, middle2 = drop middle i MSWeight o in
          (* We now know that the desired element lies in the schunk [p]. *)
          let weight_of_middle1 =
            weight_of middle - SChunk.weight p - weight_of middle2 in
          let i = i - weight_of_middle1 in
          (* Split this schunk! *)
          let x, p2 = SChunk.drop p i m o in
          (* To the right of [x], we have [p2], [middle2], and [back]. *)
          let weight2 =
            SChunk.weight p2 + weight_of middle2 + SChunk.weight back in
          let s2 = populate_nonempty_level Front weight2 p2 middle2 back o in
          x, s2

        end

(* -------------------------------------------------------------------------- *)

(* Get. *)

(* [get s i m] can be viewed as a specialized version of [three_way_split s i
   m o]. Instead of returning a triple [s1, x, s2], it returns only the pair
   [i - weight_of s1, x]. *)

let rec get : 'a. 'a t -> weight -> 'a measure -> weight * 'a =
  fun s i m ->
  match s with
  | Zero _
  | One _
  | Short _ ->
      assert false
  | Level { weight; front; middle; back } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then
        (* The desired element lies in the front schunk. *)
        SChunk.get_by_weight m front i
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then
          let i = i - weight_middle in
          (* The desired element lies in the back schunk. *)
          SChunk.get_by_weight m back i
        else
          (* The desired element lies in the middle. *)
          let i, p = get middle i MSWeight in
          SChunk.get_by_weight m p i

(* -------------------------------------------------------------------------- *)

(* Update. *)

(* Whereas [get] returns a pair of an adjusted weight-index [i] and element
   [x], [update] passes [i] and [x] to the update function [f] that it
   receives as an argument. Thus, there is a CPS-style intuition. *)

(* The code is written so as to preserve sharing: if possible, the resulting
   sequence is physically equal to [s]. This can be the case either when the
   sequence uniquely owns the schunk that is updated or when this schunk is
   shared but the update has no effect. *)

(* For simplicity, we assume that the update is weight-invariant, that is, it
   replaces an element with a new element of equal weight. This allows us to
   not update the [weight] field. The code could be generalized to allow
   changes in the weight: it should be sufficient to take an additional
   parameter [delta] and increase the weight by [delta] at every level. In
   that case, as soon as [delta] is nonzero, sharing becomes impossible. *)

let rec update : 'a. 'a measure -> owner -> 'a update -> 'a t update =
  fun m o f s i ->
  match s with
  | Zero _ ->
      assert false
  | One _
  | Short _ ->
      (* Not handled here. *)
      assert false
  | Level { weight; front; middle; back } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then
        (* The desired element lies in the front schunk. *)
        let front' = SChunk.update_by_weight m o f front i in
        if front == front' then s else
        let front = front' in
        Level { weight; front; middle; back }
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then
          let i = i - weight_middle in
          (* The desired element lies in the back schunk. *)
          let back' = SChunk.update_by_weight m o f back i in
          if back == back' then s else
          let back = back' in
          Level { weight; front; middle; back }
        else
          (* The desired element lies in the middle sequence. *)
          let middle' =
            update MSWeight o (
              SChunk.update_by_weight m o f
            ) middle i
          in
          if middle == middle' then s else
          let middle = middle' in
          Level { weight; front; middle; back }

(* Set. *)

let[@inline] set s i m o x' =
  let f x i =
    (* [x] and [x'] must have the same weight. *)
    assert (apply m x = apply m x');
    (* The weight index [i] must fall inside [x],
       and inside [x'], which is the same thing. *)
    assert (0 <= i && i < apply m x);
    x'
  in
  update m o f s i

(* For the record, a naive implementation of [set]:

let set s i m o x =
  let s1, _, s2 = three_way_split s i m o in
  concat s1 (push Front s2 x m o) o

 *)

(* -------------------------------------------------------------------------- *)

(* Public bindings. *)

let weight =
  weight_of

end (* Make *)
