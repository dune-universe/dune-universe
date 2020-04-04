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
| MeasureUnit         : 'a measure
| MeasureSchunkWeight : 'a schunk measure

let apply =
  SChunk.apply

(* -------------------------------------------------------------------------- *)

(* A shareable chunk sequence is organized in several levels. It can be
   thought of as a list of levels, where the constructors [Empty] and [Level]
   play the roles of nil and cons. *)

(* The number of levels is logarithmic in the number of elements in the
   sequence. *)

(* The data constructor [Zero] represents an empty sequence. It carries a
   [default] element, which is used when creating an empty chunk. *)

(* Both [Zero] and [Level] carry a [depth] field. This information is useful,
   for instance, when we wish to allocate a new chunk, whose capacity should
   be [capacity depth]. *)

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

type 'a t =
  | Zero of {
      default : 'a;
      depth : depth;
    }
  | Level of {
      weight : weight;
      front : 'a schunk;
      middle : 'a schunk t;
      back : 'a schunk;
      depth : depth;
    }

(* -------------------------------------------------------------------------- *)

(* Basic accessors. *)

let[@inline] default s =
  match s with
  | Zero { default; _ } ->
      default
  | Level { front; _ } ->
      SChunk.default front

(* [depth_of] is so named to avoid a clash with the field name [depth]. *)

let depth_of s =
  match s with
  | Zero { depth; _ }
  | Level { depth; _ } ->
      depth

(* [weight_of] is so named to avoid a clash with the field name [weight].
   It is ultimately exported under the name [weight]. *)

let[@inline] weight_of s =
  match s with
  | Zero _ ->
      0
  | Level { weight; _ } ->
      weight

let[@inline] is_empty s =
  match s with
  | Zero _ ->
      true
  | Level _ ->
      false

(* -------------------------------------------------------------------------- *)

(* Construction and destruction. *)

let create default depth =
  Zero { default; depth }

let[@inline] create_middle default depth =
  let default = SChunk.dummy default
  and depth = depth + 1 in
  create default depth

(* The structure of a sequence is symmetric: front and back can be exchanged,
   yielding the same structure. This allows us to share code by parameterizing
   certain operations with a point of view. *)

(* We use the words [this] and [that] to refer to this side -- the one closest
   to us, from our point of view -- and that side -- the other side. *)

let[@inline] swap pov this that =
  match pov with
  | Front ->
      this, that
  | Back ->
      that, this

(* [nonempty_level] is a constructor, parameterized by a point of view. As we
   do not allow the constructor [Level] to represent an empty sequence, this
   constructor cannot be used in the case where the sequence is empty. *)

let[@specialise] nonempty_level pov weight this middle that depth =
  assert (weight > 0);
  let front, back = swap pov this that in
  Level { weight; front; middle; back; depth }

(* [level] is also a constructor, parameterized by a point of view. It
   does not require the sequence to be nonempty. It does not guarantee
   that the populated-sides invariant holds. *)

let[@inline] level pov weight this middle that depth =
  if weight = 0 then
    let default = SChunk.default this in
    create default depth
  else
    nonempty_level pov weight this middle that depth

(* [promote p depth o] converts the schunk [p] to a sequence. *)

let promote p depth o =
  let default = SChunk.default p in
  let weight = SChunk.weight p in
  (* An empty front schunk. *)
  let front = SChunk.create default (capacity depth) o in
  (* An empty middle sequence. *)
  let middle = create_middle default depth in
  (* The schunk [p] becomes the back schunk. *)
  let back = p
    (* or: [SChunk.copy p o], but that would be more expensive *)
  in
  level Front weight front middle back depth

(* [destruct pov front back] returns either [front, back] or [back, front],
   depending on the point of view [pov]. It is so named because it is used
   when perform case analysis (also known as destruction) on a level. *)

let[@inline] destruct pov front back =
  swap pov front back

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let[@specialise] rec iter : 'a. pov -> ('a -> unit) -> 'a t -> unit =
  fun pov g s ->
  match s with
  | Zero _ ->
      ()
  | Level { front; middle; back; _ } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = destruct pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      SChunk.iter pov g this;
      iter pov (fun p -> SChunk.iter pov g p) middle;
      SChunk.iter pov g that

let iter_left g s =
  iter Front g s

let iter_right g s =
  iter Back g s

let fold_left f seed s =
  Adapters.fold_left iter_left f seed s

let to_list s =
  Adapters.to_list iter_right s

let iter_ranges s yield =
  match s with
  | Zero _ ->
      ()
  | Level { front; middle; back; _ } ->
      SChunk.iter_ranges front yield;
      iter_left (fun p -> SChunk.iter_ranges p yield) middle;
      SChunk.iter_ranges back yield

(* -------------------------------------------------------------------------- *)

(* Conversion to an array. *)

(* We assume zero depth and unit weight. Thus, the weight of the sequence
   is also its length. *)

let to_array s =
  assert (depth_of s = 0);
  ArrayExtra.concat_segments (default s) (weight_of s) (iter_ranges s)

(* -------------------------------------------------------------------------- *)

(* Printing. *)

let rec print : 'a. ('a -> PPrint.document) -> 'a t -> PPrint.document =
  fun element s ->
  let open PPrint in
  let open PPrint.OCaml in
  match s with
  | Zero _ ->
      !^ "Zero"
  | Level { front; middle; back; weight; _ } ->
      let schunk = SChunk.print element in
      !^ "Level " ^^ record "pwseq" [
        "weight", int weight;
        "front", schunk front;
        "middle", print schunk middle;
        "back", schunk back;
        "model", flowing_list element (to_list s);
      ]

(* -------------------------------------------------------------------------- *)

(* Validation. *)

let rec check : 'a. 'a t -> 'a measure -> owner -> depth -> unit =
  fun s m o expected_depth ->
  match s with
  | Zero { depth; _ } ->

      (* Check that the depth is correct. *)
      assert (depth = expected_depth)

  | Level { weight; front; middle; back; depth } ->

      (* Since the sequence is nonempty, its weight must be positive. *)
      assert (weight > 0);
      (* Check that the depth is correct. *)
      assert (depth = expected_depth);
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
  check middle MeasureSchunkWeight o (depth + 1);
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

let[@inline] check s m o expected_depth =
  assert (check s m o expected_depth; true)

(* [check_owners s o] checks that the creator of every schunk in the
   sequence [s] is less than or equal to [o] according to the ordering
   relation on owners. It is used only for debugging purposes. *)

let rec check_owners : 'a . 'a t -> owner -> unit =
  fun s o ->
  match s with
  | Zero _ ->
      ()
  | Level { front; middle; back } ->
      SChunk.check_owners front o;
      SChunk.check_owners back o;
      iter Front (fun p -> SChunk.check_owners p o) middle;
      check_owners middle o

(* -------------------------------------------------------------------------- *)

(* Peek. *)

let[@specialise] peek pov s =
  match s with
  | Zero _ ->

      raise Empty

  | Level { front; back; _ } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = destruct pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      if not (SChunk.is_empty this) then
        SChunk.peek pov this
      else
        SChunk.peek pov that

(* -------------------------------------------------------------------------- *)

(* Push. *)

let[@specialise] rec push : 'a. pov -> 'a t -> 'a -> 'a measure -> owner -> 'a t =
  fun pov s x m o ->
  match s with
  | Zero { default; depth } ->

      (* Create an empty schunk. *)
      let p = SChunk.create default (capacity depth) o in
      (* Push [x] into it. *)
      let p = SChunk.push pov p x m o in
      (* Promote it to a sequence. *)
      promote p depth o

  | Level { weight; front; middle; back; depth } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = destruct pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      let weight = weight + apply m x in
      if not (SChunk.is_full this) then begin
        (* There is room in the front schunk. Push [x] into it. *)
        let this = SChunk.push pov this x m o in
        nonempty_level pov weight this middle that depth
      end
      else if SChunk.is_empty that then begin
        (* The front schunk is full. *)
        (* The back schunk is empty. Per the populated-sides invariant,
           the middle sequence must be empty as well. *)
        assert (is_empty middle);
        (* Push [x] into the back schunk and exchange the front and back
           schunks, so [x] ends up in front, as desired. *)
        let this, that = SChunk.push pov that x m o, this in
        nonempty_level pov weight this middle that depth
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
          push pov middle this MeasureSchunkWeight o
        in
        nonempty_level pov weight this middle that depth
      end

(* -------------------------------------------------------------------------- *)

(* Pop. *)

let[@specialise] rec pop : 'a. pov -> 'a t -> 'a measure -> owner -> 'a * 'a t =
  fun pov s m o ->
  match s with
  | Zero _ ->

      raise Empty

  | Level { weight; front; middle; back; depth } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = destruct pov front back
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
        x, level pov weight this middle that depth
      end
      else begin
        (* Pop an element [x] off the front schunk. *)
        let x, this = SChunk.pop pov this m o in
        let weight = weight - apply m x in
        (* This test is identical to the one in [level]. *)
        if weight = 0 then
          let default = SChunk.default this in
          x, create default depth
        else
          (* Restore the populated-sides invariant. *)
          (* This is a specialized version of [populate]. *)
          let this, middle =
            if SChunk.is_empty this && not (is_empty middle)
            then pop pov middle MeasureSchunkWeight o
            else this, middle
          in
          x, nonempty_level pov weight this middle that depth
      end

(* -------------------------------------------------------------------------- *)

(* Restoring the populated-sides invariant. *)

(* These functions use [pop], which is why they are placed here. *)

(* [populate pov s o] restores one side of the populated-sides invariant. *)

let[@inline] populate pov s o =
  match s with
  | Zero _ ->
      s
  | Level { weight; front; middle; back; depth } ->
      (* Rename [front] and [back] to [this] and [that],
         with a possible exchange, depending on [pov]. *)
      let this, that = destruct pov front back
      (* Ensure [front] and [back] are not used below. *)
      and front, back = (), () in front; back;

      if SChunk.is_empty this && not (is_empty middle) then begin
        (* The invariant is broken on this side: the front chunk is
           empty, yet the middle sequence is nonempty. Restore the
           invariant by popping one element off the middle sequence
           and making it the front chunk. *)
        let this, middle = pop pov middle MeasureSchunkWeight o in
        assert (not (SChunk.is_empty this));
        nonempty_level pov weight this middle that depth
      end
      else
        s

(* The smart constructor [populate_both_nonempty_level] requires the sequence
   to be nonempty and restores the populated-sides invariant if necessary. It
   is not parameterized by a point of view. *)

let populate_both_nonempty_level weight front middle back o depth =
  assert (weight > 0);
  let s = Level { weight; front; middle; back; depth } in
  let s = populate Front s o in
  let s = populate Back s o in
  s

(* The smart constructor [populate_nonempty_level] requires the sequence to be
   nonempty and restores one side of the populated-sides invariant. *)

let[@specialise] populate_nonempty_level pov weight front middle back o depth =
  assert (weight > 0);
  let s = Level { weight; front; middle; back; depth } in
  let s = populate pov s o in
  s

(* The smart constructor [populate_level] supports the case where the sequence
   is empty and restores one side of the populated-sides invariant. *)

let[@inline] populate_level pov weight front middle back o depth =
  if weight = 0 then
    let default = SChunk.default front in
    create default depth
  else
    populate_nonempty_level pov weight front middle back o depth

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
    create default depth
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
    let middle = ref (create_middle default depth) in
    foreach_middle_segment (fun i k ->
      middle := push Back !middle (create_schunk (i, k)) MeasureSchunkWeight o
    );
    let middle = !middle in
    (* Construct the back. *)
    let back = create_schunk back in
    (* Build a level. Because [cut] is designed to enforce the populated-sides
       invariant by construction, there is no need to call [populate]. *)
    Level { weight; front; middle; back; depth }
  end

(* Conversion from an array. *)

let of_array_segment default a head size o =
  assert (ArrayExtra.is_valid_segment a head size);
  create_by_segments default size o (fun n i k ->
    (* Construct the segment [i, i + k) of the sequence using the elements in
       the segment [head + i, head + i + k) of the array [a]. *)
    SChunk.of_array_segment default n a (head + i) k o
  )

let make default size v o =
  assert (0 <= size);
  create_by_segments default size o (fun n _i k ->
    (* Allocate a schunk of capacity [n] and size [k],
       filled with [k] copies of the value [v]. *)
    SChunk.make default n k v o
  )

let init default size f o =
  assert (0 <= size);
  create_by_segments default size o (fun n i k ->
    (* Allocate a schunk of capacity [n] and size [k],
       filled with the values [f i], [f (i+1)], ..., [f (i+k-1)]. *)
    SChunk.init default n k i f o
  )

(* -------------------------------------------------------------------------- *)

(* [fuse_back middle p o] pushes the schunk [p] onto the back of [middle],
   which is a sequence of schunks. If possible, the last schunk of the
   sequence and the schunk [p] are fused into a single schunk. This allows
   preserving the density invariant. *)

let[@inline] fuse_back middle p o =
  if SChunk.is_empty p then
    (* [p] is empty. There is nothing to do. *)
    middle
  else if is_empty middle then
    (* [middle] is empty. Just push [p] into [middle]. *)
    push Back middle p MeasureSchunkWeight o
  else
    (* The capacity at the current depth is given by [SChunk.capacity p]. *)
    let n = SChunk.capacity p in
    if SChunk.length (peek Back middle) + SChunk.length p > n then
      (* Concatenating the last element of [middle] with [p] is forbidden.
         Just push [p] into [middle]. *)
      push Back middle p MeasureSchunkWeight o
    else begin
      (* [middle] is nonempty and its last element [last] can be concatenated
         with [p]. Then, pop [last] off [middle], concatenate [last] and [p],
         and push the result back into [middle]. *)
      let last, middle = pop Back middle MeasureSchunkWeight o in
      let last = SChunk.concat last p o in
      push Back middle last MeasureSchunkWeight o
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

let rec concat : 'a. 'a t -> 'a t -> owner -> 'a t =
  fun s1 s2 o ->
  match s1, s2 with
  | Zero _, s
  | s, Zero _ ->
      s
  | Level { weight = weight1; front = front1; middle = middle1;
            back = back1; depth = depth1 },
    Level { weight = weight2; front = front2; middle = middle2;
            back = back2; depth = depth2; } ->
      (* Weight. *)
      let weight = weight1 + weight2 in
      (* Depth. *)
      assert (depth1 = depth2);
      let depth = depth1 in
      (* Exchange the front and back schunks of [s1], if necessary, to ensure
         that its front schunk is nonempty. *)
      let front1, back1 = eject front1 middle1 back1 in
      assert (not (SChunk.is_empty front1));
      (* Similarly, ensure that the back schunk of [s2] is nonempty. *)
      let back2, front2 = eject back2 middle2 front2 in
      assert (not (SChunk.is_empty back2));
      (* Get rid of [back1] and [front2] by pushing them into [middle1]. *)
      let middle1 = fuse_back middle1 back1 o in
      let middle1 = fuse_back middle1 front2 o in
      (* There remains to concatenate the two middles, *)
      let middle = fuse middle1 middle2 o in
      (* and build a new level. *)
      populate_both_nonempty_level weight front1 middle back2 o depth

and fuse : 'a. 'a schunk t -> 'a schunk t -> owner -> 'a schunk t =
  fun s1 s2 o ->
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
        concat s1 s2 o
      else
        let first2, s2 = pop Front s2 MeasureSchunkWeight o in
        let s1 = fuse_back s1 first2 o in
        concat s1 s2 o

(* -------------------------------------------------------------------------- *)

(* Split. *)

let rec three_way_split :
  'a. 'a t -> weight -> 'a measure -> owner -> 'a t * 'a * 'a t
=
  fun s i m o ->
  match s with
  | Zero _ ->
      assert false
  | Level { weight; front; middle; back; depth } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then begin

        (* The desired element lies in the front schunk. *)
        let front1, x, front2 = SChunk.three_way_split front i m o in
        let s1 = promote front1 depth o in
        let weight2 = weight - weight_of s1 - apply m x in
        let s2 = populate_level Front weight2 front2 middle back o depth in
        s1, x, s2

      end
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then begin
          let i = i - weight_middle in

          (* The desired element lies in the back schunk. *)
          let back1, x, back2 = SChunk.three_way_split back i m o in
          let s2 = promote back2 depth o in
          let weight1 = weight - weight_of s2 - apply m x in
          let s1 = populate_level Back weight1 front middle back1 o depth in
          s1, x, s2

        end
        else begin

          (* The desired element lies in the middle. *)
          (* Split the middle sequence. *)
          let middle1, p, middle2 = three_way_split middle i MeasureSchunkWeight o in
          (* We now know that the desired element lies in the schunk [p]. *)
          let i = i - weight_of middle1 in
          (* Split this schunk! *)
          let p1, x, p2 = SChunk.three_way_split p i m o in
          (* To the left of [x], we have [front], [middle1], and [p1]. *)
          let weight1 = SChunk.weight front + weight_of middle1 + SChunk.weight p1 in
          let s1 = populate_nonempty_level Back weight1 front middle1 p1 o depth in
          (* To the right of [x], we have [p2], [middle2], and [back]. *)
          let weight2 = SChunk.weight p2 + weight_of middle2 + SChunk.weight back in
          let s2 = populate_nonempty_level Front weight2 p2 middle2 back o depth in
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

(* Get. *)

(* [get s i m] can be viewed as a specialized version of [three_way_split s i
   m o]. Instead of returning a triple [s1, x, s2], it returns only the pair
   [i - weight_of s1, x]. *)

let rec get : 'a. 'a t -> weight -> 'a measure -> weight * 'a =
  fun s i m ->
  match s with
  | Zero _ ->
      assert false
  | Level { weight; front; middle; back; _ } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then
        (* The desired element lies in the front schunk. *)
        SChunk.get_by_weight front i m
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then
          let i = i - weight_middle in
          (* The desired element lies in the back schunk. *)
          SChunk.get_by_weight back i m
        else
          (* The desired element lies in the middle. *)
          let i, p = get middle i MeasureSchunkWeight in
          SChunk.get_by_weight p i m

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
  | Level { weight; front; middle; back; depth } ->
      assert (0 <= i && i < weight);
      let weight_front = SChunk.weight front in
      if i < weight_front then
        (* The desired element lies in the front schunk. *)
        let front' = SChunk.update_by_weight m o f front i in
        if front == front' then s else
        let front = front' in
        Level { weight; front; middle; back; depth }
      else
        let i = i - weight_front in
        let weight_middle = weight_of middle in
        if weight_middle <= i then
          let i = i - weight_middle in
          (* The desired element lies in the back schunk. *)
          let back' = SChunk.update_by_weight m o f back i in
          if back == back' then s else
          let back = back' in
          Level { weight; front; middle; back; depth }
        else
          (* The desired element lies in the middle sequence. *)
          let middle' =
            update MeasureSchunkWeight o (
              SChunk.update_by_weight m o f
            ) middle i
          in
          if middle == middle' then s else
          let middle = middle' in
          Level { weight; front; middle; back; depth }

(* Set. *)

let[@inline] set s i m o x =
  let f _x i = assert (i = 0); x in
  update m o f s i

(* For the record, a naive implementation of [set]:

let set s i m o x =
  let s1, _, s2 = three_way_split s i m o in
  concat s1 (push Front s2 x m o) o

 *)

(* -------------------------------------------------------------------------- *)

(* Iterators. *)

module Iter = struct

 (* The iterator module offers an interface to navigate through elements,
    and to get a handle in log-time on an specific element from the time,
    from which navigation in both direction is possible in
    amortized constant time (worst-case log-time).

    The most basic iteration pattern is:

      let it = Iter.create front s in
      while Iter.has_next it do
         let x = Iter.next it in
         ..
      done

    It is also possible to query [next] without checking [has_next], and
    catch an exception to detect the end.

       begin try while true do
         let x = try Iter.next it with Not_found -> raise Break in
         ...
       done with Break -> () end

    Alternatively, one can use [next_opt] to detect the end.

       begin try while true do
         let x = match Iter.next_opt it with
                 | Some x -> x
                 | None -> raise Break in
         ...
       done with Break -> () end

    *)

  (* An iterator points either on one element from the sequence, or on one
     virtual element "one-past-the-end". The constructors for an iterator
     include:
     - [Iter.create front] to start before the front element
     - [Iter.create back] to start after the back element
     - [Iter.create_at w] to reach the element covering the weighted index [w].

     The operation [Iter.has pov it] checks whether there exists a (real) element
     ahead of the current element, in the specified direction of navigation.

     The operation [Iter.get it] reads the currently focused element, or returns
     [Not_found] if the iterator points on the virtual front or back element.

     The operation [Iter.move pov it] shifts the iterator to make it point to
     the next element in the iteration, and returns that element. It raises
     [Not_found] if there are no such element, that is, if the iterator reaches
     a virtual "one-past-the-end" element.

     Warning: [move front] means "move from front to back" (and not "move
     towards front"), thus it means [next]. Likewise, [move back] means [prev].

     The implementation is optimized in such a way to minimize the cost for
     moving the iterator inside a same schunk.

     The iterator maintains the total weight to the left of the current cursor
     position.
     (TODO: for unit-weight sequence, there is no need to maintain this field,
           one only needs to maintain the weight before the current chunk,
           then add the index.)

     Unless it points on the virtual front and back elements, the iterator
     points on a schunk, with an offset in that schunk.

     The schunk under focus may be the front chunk, the back chunk, or one of
     the chunks stored in the middle sequence. In the latter case, we use an
     iterator on the middle sequence to be able to navigate forward and
     backward in that sequence.

     Because the middle sequence is itself a sequence, the iterator on the
     middle sequence is itself an iterator, at a different type.

     The iterator stores a pointer on the structure that it iterates over, so
     as to be able to navigate between the front, middle, and back fields from
     the sequence.
   *)

  (* A [path] object describes where the iterator points in the sequence:
     - either to the Front or Back of the sequence
     - or to an element from the front schunk or the back schunk of the sequence
     - or to an element from a schunk stored in the middle sequence.

      An [iter] object describes an iterator on a sequence.

     - [seq] is the sequence on which the iterator operators. (This sequence
       must be of the form [Level _], since it is nonempty.)

     - [path] is a path object that describes the position of the iterator,
       as explained above.

     - [schunk] is the schunk under focus, that is, the schunk that contains
       the currently-focused element, if any. If [path] denotes a virtual element
       at the end of the sequence, then [schunk] is a dummy schunk (the fact that
       a dummy schunk has length zero is exploited in the critical section of
       the implementation of [move]).

     - [index] is the index within [schunk] of the current element. (This index
       is relative to the view segment, not to the underlying support array.)
       If [path] denotes a virtual element at the end of the sequence, then
       [index] is equal to zero (this property is exploited in the critical
       section of the implementation of [move]).

     - [windex] stores the cumulated weight of the elements found (strictly)
       to the left of the current element (with respect to the complete sequence).
       In other words, it is the weight-index of the current element.
       If [path] denotes the virtual element at the front of the sequence, then
       [windex] stores zero. If [path] denotes the virtual element at the back of
       the sequence, then [windex] stores the total weight of the sequence.

     In the case of an empty sequence, the record fields are filled with dummy
     values. In particular, [schunk] is a dummy chunk, which has the remarkable
     property of being considered both empty and full. This allows us to optimize
     the code for the typical case of nonempty sequences. *)

  type 'a iter = {
    seq : 'a t; (* TODO: later, seq can be mutable if we perform updates through the iterator *)
    mutable path : 'a path;
    mutable schunk : 'a schunk;
    mutable index : index;
    mutable windex : weight;
  }

  (* TODO I think all the operations should take the measure [m] as argument,
     because, depending on the implementation details, it may or may not
     be needed.*)

  (* A [path] describes the location of the focused schunk inside the sequence.
     A path is:

      - [PathEnd pov], to describe a virtual element one-past-the-end of the
        first/last element from the sequence. (For an empty sequence,
        [PathEnd Front] and [PathEnd Back] play equivalent roles.)

      - [PathSide pov], to describe the [Front] schunk or the [Back] schunk;

      - [PathMiddle it], where [it] is an iterator on the middle sequence,
        to describe a schunk stored in the middle sequence. *)

  and 'a path =
    | PathEnd of pov
    | PathSide of pov
    | PathMiddle of 'a schunk iter

  (* Accessors *)

  let sequence it =
    it.seq

  let windex it =
    it.windex

  (* [is_at_end it] returns true if the iterator points on a one-past-the-end
     element, that is, if [get it] would raise an exception. *)

  let is_at_end it =
    match it.path with
    | PathEnd _ -> true
    | _ -> false

  (* Get operation *)

  let get it =
    if is_at_end it
      then raise Not_found
      else SChunk.get it.schunk it.index

  (* Creation of an iterator at the virtual front/back element at an end of the
      sequence. *)

  let create pov s _m =
    let windex = match pov with Front -> 0 | Back -> weight_of s in
    { seq = s;
      path = PathEnd pov;
      schunk = SChunk.dummy (default s);
      index = 0;
      windex = windex; }

  (* [rewind] moves the iterator to the end of the sequence, e.g.,
     [rewind Front] moves the iterator to the virtual element at the front.
     Note: [rewind pov] sets the iterator fields exactly like [create pov]. *)

  let rewind pov it _m =
    let windex = match pov with Front -> 0 | Back -> weight_of it.seq in
    it.path <- PathEnd pov;
    if not (SChunk.is_dummy it.schunk)
      then it.schunk <- SChunk.dummy (default it.seq);
    it.index <- 0;
    it.windex <- windex

  (* [update_fields it path p j windex] is an auxiliary function to assign
     the fields of an iterator. *)

  let update_fields it path p j windex =
    it.path <- path;
    it.schunk <- p;
    it.index <- j;
    it.windex <- windex

  (* [reach it i m] must be invoked with an (weighted) index [i]
     [-1 <= i <= weight s]. It moves the iterator to the element,
     possibly to the one-past-the-end element, that covers the weighted
     index [i].

     TODO: including -1 and weight s might be useful for unit-weighted
     seqeuences. it's also possible to remove this feature, thought.

     Remark: [reach it -1 m] is equivalent to [rewind Front it m],
     and [reach it (weight s) m] is equivalent to [rewind Back it m].

     The cost of [reach] is O(log d), where [d] denotes the distance
     (counted in number of elements) between the current element
     and the targeted item. *)

  let rec reach : 'a. 'a iter -> weight -> 'a measure -> unit =
  fun it i m ->
    assert (-1 <= i && i <= weight_of it.seq);
    if i = -1 then rewind Front it m
    else if i = weight_of it.seq then rewind Back it m
    else
    match it.seq with
    | Zero _ -> assert false
    | Level { weight; front; middle; back; _ } ->
        assert (0 <= i && i < weight);
        let weight_front = SChunk.weight front in
        if i < weight_front then begin
          (* The desired element lies in the front schunk. *)
          let weight_front1, j = SChunk.find_weight_index front i m in
          update_fields it (PathSide Front) front j weight_front1
        end else begin
          let i = i - weight_front in
          let weight_middle = weight_of middle in
          if weight_middle <= i then begin
            let i = i - weight_middle in
            (* The desired element lies in the back schunk. *)
            let weight_back1, j = SChunk.find_weight_index back i m in
            let windex = weight_front + weight_middle + weight_back1 in
            update_fields it (PathSide Back) back j windex
          end else begin
            (* The desired element lies in the middle. *)
            let it_middle =
              (* Either create or update an iterator on middle *)
              match it.path with
              | PathMiddle it_middle -> reach it_middle i MeasureSchunkWeight; it_middle
              | _ -> create_at middle i MeasureSchunkWeight
              in
            assert (not (is_at_end it_middle));
            let p = get it_middle in
            let weight_middle1 = windex it_middle in
            let i = i - weight_middle1 in
            let weight_p1, j = SChunk.find_weight_index p i m in
            let windex = weight_front + weight_middle1 + weight_p1 in
            update_fields it (PathMiddle it_middle) p j windex
          end
       end

  (* Creation at an arbitrary position of the sequence.
     [create_at s i w] must be invoked with an (weighted) index [i]
     such that [-1 <= i <= weight s].

     Remark: [create_at s (-1)] is equivalent to [create Front s] and
     [create_at s (weight s)] is equivalent to [create Back s].

     Remark: for an empty sequence [s], [create_at s 0] does not fail. *)

  and create_at : 'a. 'a t -> weight -> 'a measure -> 'a iter =
  fun s i m ->
    let it = create Front s m in
    reach it i m;
    it

  (* Implementation for [has_next] (encoded as [has Front])
     and [has_prev] (encoded as [has Back]). *)

  let has : 'a. pov -> 'a iter -> 'a measure -> bool =
    fun pov it m ->
    match it with { path; schunk; index; _ } ->
      (* Code for testing if the sequence has elements beyond the current schunk. *)
       let has_in_rest () =
          match path with
          | PathEnd it_pov ->
              (* If the iterator is currently on a virtual element of the sequence,
                 there are more elements if the direction of traversal
                 is towards the opposite end, and the sequence is not empty. *)
              (it_pov = pov) && not (is_empty it.seq)
          | _ ->
              (* If the iterator is not on a virtual element of the sequence,
                 the [windex] describes the weight to the left of the current
                 element, and we can use [windex] to find if there are elements
                 to the left/right of the iterator. Note that we exploit here
                 the fact that elements have nonzero weights. Recall that [windex]
                 denotes the weight strictly to the left of the current item. *)
              match pov with
              | Front -> it.windex + apply m (get it) < weight_of it.seq
              | Back -> it.windex > 0
        in
      (* First, we test if the current schunk has more elements.
         Else, we call the function [has_in_rest].
         If the path is a [PathEnd] (in particular, this is the necessarily
         the case if the sequence is empty), then [schunk] is a dummy chunk,
         which is of length zero, and [index] is zero, thus the
         [has_in_current_schunk] evaluates to false.
         This is a critical section that should remained optimized as is. *)
      let has_in_current_schunk =
        match pov with
        | Front -> let n = SChunk.length schunk in (index < n-1)
        | Back -> (index > 0)
        in
      has_in_current_schunk || has_in_rest()

  (* Implementation for [move].

     This function returns the newly-focused element.
     It raises [Not_found] if reaching a one-past-the-end element.
     In that case, the iterator is still valid, e.g. and one can still
     call [move (dual pov) it] to come back to the previous point
     in the iterationn, and the windex is updated as it should
     (e.g. [weight_of s] for a [move Back] that reaches the end).

     If the iterator is already on a one-past-the-end element and
     [move] is called in the direction away from the elements, then
     [Not_found] is raised and the state of the iterator is not updated.

     TODO: it's rather unusual for side-effect to be performed before an
     exception is raised, so this should be well documented. *)

  let rec move : 'a. pov -> 'a iter -> 'a measure -> 'a =
    fun pov it m ->
      match it with { schunk; index; windex; seq; path } ->
        (* Code to move the iterator to the next schunk, when reaching
           the end of the current schunk. *)
        let move_to_next_schunk () =
          let (this, that), middle =
            match seq with
            | Zero _ -> assert false
            | Level { front; middle; back; _ } -> destruct pov front back, middle
          in
          (* Function to update the iterator, by setting the [path], and focusing
              on the first/last element of the schunk [p], taking care to update
              the [index] and [windex] fields appropriately. *)
          let update_path_and_schunk path p =
            let j =
              match pov with
              | Front -> 0
              | Back -> (SChunk.length p) - 1 in
            let x = SChunk.get p j in
            let delta =
              match pov with
              | Front -> apply m (get it) (* weight of currently-focused element *)
              | Back -> - apply m x (* weight of newly-focused element *)
              in
            update_fields it path p j (windex + delta);
            x
          in
          (* Function for moving the iterator to the schunk [that]. *)
          let move_into_that () =
            if not (SChunk.is_empty that) then begin
              (* The [that] chunk is not empty, so we focus on its first element. *)
              update_path_and_schunk (PathSide (dual pov)) that
            end else begin
              (* The [that] chunk is empty, so we go one-past-the-end. *)
              rewind (dual pov) it m;
              raise Not_found
            end
          in
          match path with
          | PathEnd it_pov ->
              if it_pov <> pov then begin
                (* The iterator is already beyond the limit of the array *)
                raise Not_found
              end else if not (SChunk.is_empty this) then begin
                (* The first chunk is the [this] schunk, so we move to it *)
                  update_path_and_schunk (PathSide pov) this
              end else
                (* The schunk [this] is empty, so is [middle], so move to [that] *)
                move_into_that()
          | PathSide it_pov ->
              if pov <> it_pov then begin
                (* The iterator reached the end of the last chunk, so we go one-past-the-end. *)
                rewind it_pov it m;
                raise Not_found
              end
              (* The iterator is currently at the end of the first chunk of the sequence. *)
              else if not (is_empty middle) then begin
                (* [middle] is nonempty, so we move to the head schunk from [middle]. *)
                let it_middle = create pov middle MeasureSchunkWeight in
                let p = get it_middle in
                update_path_and_schunk (PathMiddle it_middle) p
              end
              else
                (* [middle] is empty, so we move to the schunk [that]. *)
                move_into_that()
          | PathMiddle it_middle ->
              (* The iterator reached the end of a chunk from the middle sequence. *)
              if has pov it_middle MeasureSchunkWeight then begin
                (* [middle] contains a next schunk, so we move into it. *)
                let p = move pov it_middle MeasureSchunkWeight in
                update_path_and_schunk path p
              end
              else
                (* The iterator reached the end of [middle], so we move to [that]. *)
                move_into_that()
        in
        (* First, we attempt moving the iterator within the current schunk.
           If we reach the end of it, we call the function [move_to_next_schunk].
           Note that if the sequence is empty, then the [schunk] has length zero,
           and the function [move_to_next_schunk] is called.
           This is a critical section that should remained optimized as is. *)
        match pov with
        | Front ->
            let n = SChunk.length schunk in
            if index < n-1 then begin
              let current = get it in
              it.windex <- it.windex + apply m current;
              it.index <- index + 1;
              get it
            end
            else
              move_to_next_schunk()
              (* we reached the end of the current schunk, so we move to the next one *)
        | Back ->
            if index > 0 then begin
              it.index <- index - 1;
              let x = get it in
              it.windex <- it.windex - apply m x;
              x
            end
            else
              move_to_next_schunk()
              (* we reached the end of the current schunk, so we move to the next one *)

  (* TODO: move this wrapper to Sek, there is no need to have it here *)
  let move_opt pov it m =
    try Some (move pov it m)
    with Not_found -> None
    (* Alternative code, for verification without exceptions:
      [if has pov it m then Some (move pov it m) else None] *)

  (* The [copy] function enables forking an iterator more efficiently than with
     a call to [create_at]. *)

  let rec copy : 'a. 'a iter -> 'a iter =
    fun it ->
    { it with path = path_copy it.path }

  and path_copy : 'a. 'a path -> 'a path =
    fun path ->
    match path with
    | PathEnd _ | PathSide _ -> path
    | PathMiddle it_middle -> PathMiddle (copy it_middle)

end (* end Iter *)

let weight =
  weight_of

end (* Make *)
