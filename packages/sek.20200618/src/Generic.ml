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

(* This file contains "generic" implementations of the operations that do not
   need any internal knowledge of our data structures. *)

open PrivateSignatures
include PublicSignature

(* -------------------------------------------------------------------------- *)

(* Logging support. *)

let debug =
  false

let[@inline] log format =
  if debug then
    Printf.fprintf stderr format
  else
    Printf.ifprintf stderr format

(* -------------------------------------------------------------------------- *)

(* Implementations of [find] and friends in terms of [iter]. *)

module[@inline] Iter (S : sig
  type 'a t
  val iter : direction -> ('a -> unit) -> 'a t -> unit
end) = struct

  open S

  let[@specialise] find (type a) direction (p : a -> bool) (s : a t) : a =
    (* [let exception] requires OCaml 4.04. *)
    let module E = struct
      exception Found of a
    end in
    match
      iter direction (fun x ->
        if p x then
          raise (E.Found x)
      ) s
    with
    | exception E.Found x ->
        x
    | () ->
        raise Not_found

  let[@specialise] find_opt direction p s =
    try
      Some (find direction p s)
    with Not_found ->
      None

  let[@specialise] find_map (type a b)
    direction (p : a -> b option) (s : a t) : b option
  =
    (* [let exception] requires OCaml 4.04. *)
    let module E = struct
      exception Found of b option
    end in
    match
      iter direction (fun x ->
        match p x with
        | Some _ as o ->
            raise (E.Found o)
        | None ->
            ()
      ) s
    with
    | exception E.Found o ->
        o
    | () ->
        None

  let exists p s =
    try
      ignore (find forward p s);
      true
    with Not_found ->
      false

  let[@inline] for_all p s =
    not (exists (fun x -> not (p x)) s)

  let[@inline] mem x s =
    exists (fun y -> x = y) s

  let[@inline] memq x s =
    exists (fun y -> x == y) s

end (* Iter *)

(* -------------------------------------------------------------------------- *)

(* Implementations of [filter], [partition], and friends in terms of [iter],
   [create], [push], and [finalize]. When the result type ['a u] is the type
   of ephemeral sequences, [finalize] is the identity function; when the
   result type is the type of persistent sequences, [finalize] is
   [snapshot_and_clear]. *)

(* In [create n default], the parameter [n] is an upper bound on the size
   of the ephemeral sequence that is being created. *)

module[@inline] IterCreatePush (S : sig
  type 'a t
  val default : 'a t -> 'a
  val length : 'a t -> length
  val iter : direction -> ('a -> unit) -> 'a t -> unit
  type 'a u
  val create : length -> 'a -> 'a u
  val push : direction -> 'a u -> 'a -> unit
  val finalize : 'a u -> 'a t
end) = struct

  open S

  let filter p s =
    let s' = create (length s) (default s) in
    iter forward (fun x ->
      if p x then
        push back s' x
    ) s;
    finalize s'

  let filter_map d f s =
    let s' = create (length s) d in
    iter forward (fun x ->
      match f x with
      | None ->
          ()
      | Some y ->
          push back s' y
    ) s;
    finalize s'

  let flatten_map d f s =
    (* Here, we would like to first compute the length [n] of the sequence
       [s'], then call [create n d]. However, because the function [f] might
       be impure, we are not allowed to call it more than once. So we cannot
       do that (or we would have to allocate a sequence of the results of
       these calls, but that would be bad if [s] is a very long sequence). *)
    (* We work around this problem by passing [max_int] as an upper bound
       and hoping that [create] ignores its argument [n] anyway. It is up
       to our caller to ensure this. *)
    let n = max_int in
    let s' = create n d in
    iter forward (fun x ->
      let ys = f x in
      iter forward (fun y ->
        push back s' y
      ) ys
    ) s;
    finalize s'

  let partition p s =
    let n = length s
    and d = default s in
    let s1, s2 = create n d, create n d in
    iter forward (fun x ->
      push back (if p x then s1 else s2) x
    ) s;
    finalize s1, finalize s2

end (* IterCreatePush *)

(* -------------------------------------------------------------------------- *)

(* An implementation of [uniq] in terms of [filter] and a few more functions. *)

(* To implement [uniq], we could use [filter is_new s], where the function
   [is_new] maintains a reference of type ['a option ref]. We prefer to avoid
   this option (which imposes a lot of memory allocation). To do so, we peek
   at the first element of [s] and use it to initialize a reference of type
   ['a ref]. *)

let[@inline] uniq is_empty create default peek filter push cmp s =
  if is_empty s then
    create (default s)
  else
    let x = peek front s in
    let previous = ref x in
    let is_new x = cmp !previous x <> 0 && (previous := x; true) in
    let s = filter is_new s in
    push front s x

(* -------------------------------------------------------------------------- *)

(* Implementations of [to_seq], [map], [iter2_segments], [iter2] and [map2] in
   terms of iterators and [init], plus implementations of several derived
   functions. *)

module[@inline] IteratorsInit (S : sig
  type 'a t
  val default : 'a t -> 'a
  val length : 'a t -> length
  val unchecked_init : 'a -> length -> (index -> 'a) -> 'a t
  module Iter : sig
    include ITER
      with type 'a t := 'a t
       and type direction := direction
    val get_writable_segment_and_jump : direction -> 'a iter -> 'a segment
  end
end) = struct

  open S.Iter
  open S
    (* [length] is sequence length, not iterator length *)

  (* [of_seq_segment]. *)

  let of_seq_segment d n xs =
    if not (0 <= n) then
      invalid_arg "of_seq_segment: invalid length"
    else
      let it = Adapters.iterator_of_seq xs in
      try
        unchecked_init d n (fun _i -> it())
      with Adapters.Exhausted ->
        invalid_arg
          "of_seq_segment: sequence has fewer elements than advertised"

  (* [of_list_segment]. *)

  let of_list_segment d n xs =
    if not (0 <= n) then
      invalid_arg "of_list_segment: invalid length"
    else
      let it = Adapters.iterator_of_list xs in
      try
        unchecked_init d n (fun _i -> it())
      with Adapters.Exhausted ->
        invalid_arg
          "of_list_segment: list has fewer elements than advertised"

  (* [of_list]. *)

  let[@inline] of_list d xs =
    let n = List.length xs in
    of_list_segment d n xs

  (* [to_seq]. *)

  let to_seq pov s =
    let it = create pov s in
    let rec produce () =
      try
        let x = get_and_move pov it in
        Seq.Cons (x, produce)
      with End ->
        Seq.Nil
    in
    produce

  (* [to_seqi]. *)

  let to_seqi pov s =
    let it = create pov s in
    let rec produce i () =
      try
        let x = get_and_move pov it in
        Seq.Cons ((i, x), produce (i + 1))
      with End ->
        Seq.Nil
    in
    produce 0

  (* [mapi]. *)

  (* TODO speed up using [iter_segments] and [push_segment] *)

  let[@inline] mapi d f s =
    let it = create forward s in
    unchecked_init d (length s) (fun i ->
      let x = get_and_move forward it in
      f i x
    )

  (* [map]. *)

  let[@inline] map d f s =
    let[@inline] f _i x = f x in
    mapi d f s

  (* [deep_copy s] is [map (default s) id s]. *)

  (* This is a "deep" copy, that is, all of the data is copied, and nothing is
     shared. The new sequence does not have exactly the same shape as the
     original sequence; its chunks are fully populated. *)

  let[@inline] deep_copy s =
    let[@inline] id x = x in
    map (default s) id s

  (* [rev]. *)

  (* TODO speed up using [iter_segments] and [push_segment] *)

  let rev s =
    let it = create backward s in
    unchecked_init (default s) (length s) (fun _i ->
      get_and_move backward it
    )

  (* [unzip] can be implemented either by iterating on the source and
     performing iterated pushes on two destinations; or by calling [map]
     twice, at the cost of iterating twice over the source sequence.
     We use the second approach. *)

  let unzip s =
    let (d1, d2) = default s in
    map d1 fst s, map d2 snd s

  (* To implement [iter2_segments], we use two iterators, which we query using
     [get_segment_and_jump]. *)

  (* We could also use [iter_segments] on one sequence
     and [get_segment_and_jump] on the other sequence,
     but that would be less symmetric,
     and experiments seem to indicate that [get_segment_and_jump] is faster
     than [iter_segments] anyway, although we cannot explain why that is. *)

  (* At the moment, we assume that the two iterators are based on the same
     type of sequences, so we cannot mix (say) an iterator on an ephemeral
     sequence and an iterator on a persistent sequence. This could be relaxed
     if necessary. *)

  (* We implement "lax" versions of these functions, where the lengths of the
     two sequences are allowed to differ. See NOTES.md for a discussion. *)

  let _segment_length ((_, _, k) : 'a segment) =
    k

  let[@specialise] truncate_segment pov size ((a, i, k) as seg) =
    assert (Segment.is_valid seg);
    assert (size < k);
    match pov with
    | Front ->
        a, i, size
    | Back ->
        a, i + k - size, size

  (* [preserving it f] executes the function [f] and takes action to preserve
     the iterator [it], which can be invalidated by [f]. *)

  (* TODO implement this in a more efficient way *)

  let[@inline] preserving it f =
    let i = index it in
    let y = f() in
    reset forward it;
    reach it i;
    y

  (* For some applications, such as [blit], it is convenient to have write
     access to the segment produced by the iterator [it2]. The parameter
     [writable2] controls this. *)

  (* A pitfall: if [it1] and [it2] are iterators on the same sequence, then
     writing through [it2] invalidates [it1]. We must be careful to preserve
     [it1]. This extra precaution is requested by the parameter [preserve1]. *)

  (* [even] is invoked when the two iterators are even, that is, when they
     have the same index. *)

  let[@specialise] rec even pov preserve1 writable2 it1 it2 f =
    (* Ask [it1] for a segment [seg1], and transition to [ahead1]. *)
    log "even: it1 = %d, it2 = %d\n%!" (index it1) (index it2);
    let seg1 = get_segment_and_jump pov it1 in
    ahead1 pov preserve1 writable2 it1 seg1 it2 f

  (* [ahead1] is invoked when the iterator [it1] has produced one more
     segment than the iterator [it2], namely the segment [seg1]. *)

  and[@specialise] ahead1 pov preserve1 writable2 it1 seg1 it2 f =
    log "ahead1 (%d): it1 = %d, it2 = %d\n%!"
      (_segment_length seg1) (index it1) (index it2);
    (* Ask [it2] for a segment [seg2]. *)
    let seg2 =
      if writable2 then begin
        (* A pitfall: if [it1] and [it2] are iterators on the same sequence,
           then writing through [it2] invalidates [it1]. We must be careful
           to preserve [it1]. *)
        if preserve1 then
          preserving it1 (fun () -> get_writable_segment_and_jump pov it2)
        else
          get_writable_segment_and_jump pov it2
      end
      else
        get_segment_and_jump pov it2
    in
    test pov preserve1 writable2 it1 seg1 it2 seg2 f

  (* [ahead2] is invoked when the iterator [it2] has produced one more
     segment than the iterator [it1], namely the segment [seg2]. *)

  and[@specialise] ahead2 pov preserve1 writable2 it1 it2 seg2 f =
    log "ahead2 (%d): it1 = %d, it2 = %d\n%!"
      (_segment_length seg2) (index it1) (index it2);
    (* Ask [it1] for a segment [seg1]. *)
    let seg1 = get_segment_and_jump pov it1 in
    test pov preserve1 writable2 it1 seg1 it2 seg2 f

  (* [test] is invoked when each of [it1] and [it2] have produced one
     segment. We must test which of the two segments is longer. *)

  and[@specialise] test pov preserve1 writable2 it1 seg1 it2 seg2 f =
    let a1, i1, k1 = seg1
    and a2, i2, k2 = seg2 in
    assert (k1 > 0 && k2 > 0);
    log "test (%d/%d): it1 = %d, it2 = %d\n%!"
      k1 k2 (index it1) (index it2);
    if k1 = k2 then begin
      (* The segments [seg1] and [seg2] have the same length. *)
      (* Pass them to the user, and transition to [even]. *)
      f seg1 seg2;
      even pov preserve1 writable2 it1 it2 f
    end
    else if k1 < k2 then begin
      (* [seg1] is shorter than [seg2]. Split [seg2]. *)
      match pov with
      | Front ->
          f seg1 (a2, i2, k1);
          ahead2 pov preserve1 writable2 it1 it2 (a2, i2 + k1, k2 - k1) f
      | Back ->
          f seg1 (a2, i2 + k2 - k1, k1);
          ahead2 pov preserve1 writable2 it1 it2 (a2, i2, k2 - k1) f
    end
    else begin
      assert (k2 < k1);
      (* [seg2] is shorter than [seg1]. Split [seg1]. *)
      match pov with
      | Front ->
          f (a1, i1, k2) seg2;
          ahead1 pov preserve1 writable2 it1 (a1, i1 + k2, k1 - k2) it2 f
      | Back ->
          f (a1, i1 + k1 - k2, k2) seg2;
          ahead1 pov preserve1 writable2 it1 (a1, i1, k1 - k2) it2 f
    end

  (* The main function: [iter2_segments]. *)

  (* By convention, iteration stops as soon as the end of the shorter
     sequence is reached. This is more useful than the convention of
     the OCaml standard library, which is to raise [Invalid_argument]
     when the two sequences have distinct lengths. *)

  let[@inline] iter2_segments pov s1 s2 f =
    let it1 = create pov s1
    and it2 = create pov s2 in
    try
      even pov false false it1 it2 f
      (* [even] cannot terminate normally. *)
    with End ->
      ()

  (* [bounded_iter2_segments] is a variant of [iter2_segments] that stops
     after a certain total size has been reached. The final segments are
     truncated if necessary so as to match [size] exactly. *)

  (* This function offers control over [preserve1] and [writable2], and takes
     two iterators as arguments, instead of two sequences: this allows the
     caller to choose the starting points. *)

  let[@specialise]
  bounded_iter2_segments pov preserve1 writable2 size it1 it2 f =
    try
      let size = ref size in
      even pov preserve1 writable2 it1 it2 (fun seg1 seg2 ->
        (* If we are done, stop. *)
        let s = !size in
        if s = 0 then
          raise End;
        (* Otherwise, compare the length [k] of the segments [seg1]
           and [seg2] with the requested size [s]. If [s] is less
           than [k], then the segments must be truncated. *)
        assert (_segment_length seg1 = _segment_length seg2);
        let k = _segment_length seg1 in
        if s < k then begin
          (* size := 0; *)
          f (truncate_segment pov s seg1) (truncate_segment pov s seg2);
          raise End
        end
        else begin
          size := s - k;
          f seg1 seg2
        end
      )
      (* [even] cannot terminate normally. *)
    with End ->
      ()

  (* [iter2]. *)

  let[@specialise] iter2 pov f s1 s2 =
    ArrayExtra.iter2 iter2_segments pov f s1 s2

  (* [map2] could be implemented in terms of [iter2], but that would require
     building the result via iterated pushes. Because the length of the result
     is known in advance, it is perhaps more efficient to use [init]. *)

  (* TODO speed up using [iter2_segments] and [push_segment] *)

  let map2 d f s1 s2 =
    let it1 = create forward s1
    and it2 = create forward s2 in
    unchecked_init d (min (length s1) (length s2)) (fun _i ->
      let x1 = get_and_move forward it1
      and x2 = get_and_move forward it2 in
      f x1 x2
    )

  (* [zip]. *)

  let zip s1 s2 =
    map2 (default s1, default s2) (fun x1 x2 -> (x1, x2)) s1 s2

  (* [fold_left2] and [fold_right2]. *)

  let fold_left2 f seed s1 s2 =
    Adapters.fold_left2 (iter2 forward) f seed s1 s2

  let fold_right2 f s1 s2 seed =
    Adapters.fold_right2 (iter2 backward) f s1 s2 seed

  (* [find2]. *)

  let[@specialise] find2
    (type a b)
    pov (p : a -> b -> bool) (s1 : a t) (s2 : b t) : a * b
  =
    (* [let exception] requires OCaml 4.04. *)
    let module E = struct
      exception Found of (a * b)
    end in
    match
      iter2 pov (fun x1 x2 ->
        if p x1 x2 then
          raise (E.Found (x1, x2))
      ) s1 s2
    with
    | exception E.Found xx ->
        xx
    | () ->
        raise Not_found

  (* [exists2]. *)

  let exists2 p s1 s2 =
    try
      ignore (find2 forward p s1 s2);
      true
    with Not_found ->
      false

  (* [for_all2]. *)

  let[@inline] for_all2 p s1 s2 =
    not (exists2 (fun x1 x2 -> not (p x1 x2)) s1 s2)

  (* [equal]. *)

  let equal p s1 s2 =
    length s1 = length s2 &&
    for_all2 p s1 s2

  (* [compare]. *)

  exception Return of int

  let compare (type a b) (cmp : a -> b -> int) (s1 : a t) (s2 : b t) : int =
    try
      (* Compare the elements [x1] and [x2] drawn synchronously from the
         two sequences. As soon as a comparison result [c] indicates that
         [x1] and [x2] are distinct, stop and return [c]. *)
      iter2 front (fun x1 x2 ->
        let c = cmp x1 x2 in
        if c <> 0 then
          raise (Return c)
      ) s1 s2;
      (* If we are still here, then no mismatch was found. There remains to
         check the lengths of the sequences. The outcome of the comparison
         between sequences is the outcome of the comparison between their
         lengths. *)
      compare (length s1) (length s2)
    with Return c ->
      c

  (* [merge] could be implemented by composing [to_seq], a merge
     operation on streams of type ['a Seq.t], and [of_seq]. *)

  (* We give a direct implementation in terms of our iterators. *)

  let merge cmp s1 s2 =
    let it1 = create forward s1
    and it2 = create forward s2 in
    unchecked_init (default s1) (length s1 + length s2) (fun _i ->
      if finished it1 then
        get_and_move forward it2
      else if finished it2 then
        get_and_move forward it1
      else
        let x1 = get it1
        and x2 = get it2 in
        let c = cmp x1 x2 in
        if c <= 0 then begin
          (* [x1] is smaller or equal. Pick [x1]. *)
          (* When [x1] and [x2] are equal, OCaml's [List.merge] picks [x1];
             we do the same. *)
          move forward it1;
          x1
        end
        else begin
          move forward it2;
          x2
        end
    )

end (* IteratorsInit *)
