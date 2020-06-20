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

open PublicSettings
open PrivateSignatures

module[@inline] Make
    (S : PSEQ)
    (Iter : IITER with type 'a t = 'a S.t)
    (T : THRESHOLD)
= struct

  open T

  (* We use the following type of sequences:

     * [Zero] represents an empty sequence.

     * [One] represents a singleton sequence.

     * [Short] represents a sequence whose length is comprised between
       2 and [threshold], included.

     * [Level] represents a sequence whose length is greater than [threshold].
       This data constructor is not used in this module; it is taken care of
       by the modules ShareableSequence and PersistentSequence.

     The constructors [Zero], [One], and [Short] carry a default value. *)

  (* I believe that this code works even if [threshold] is less than 2.
     In that case, the [Short] representation is simply never used. *)

  type 'a schunk = 'a S.schunk

  type 'a t = 'a S.t =
    | Zero  of { default : 'a; }
    | One   of { default : 'a; x : 'a }
    | Short of { default : 'a; a : 'a array }
    | Level of {
        weight : weight;
        front : 'a schunk;
        middle : 'a schunk t;
        back : 'a schunk;
      }

  (* Some functions have been entirely implemented in ShareableSequence
     and do not need to be re-implemented here; these include [default],
     [length], [is_empty], [iter_segments], [print], [peek]. *)

  let default =
    S.default

  let length =
    S.length

  let is_empty =
    S.is_empty

  let iter_segments =
    S.iter_segments

  let print =
    S.print

  let peek =
    S.peek

  let check s =
    match s with
    | S.Zero _
    | S.One _ ->
        ()
    | S.Short { a; _ } ->
        let n = Array.length a in
        assert (2 <= n && n <= threshold)
    | S.Level _ ->
        let n = S.length s in
        assert (n > threshold);
        S.check s

  (* Ensure [check] has zero cost in release mode. *)

  let[@inline] check s =
    assert (check s; true)

  let create default =
    S.Zero { default }

  (* [of_array_segment_one_short] is a special case of [of_array_segment]
     with the additional precondition [0 < k && k <= threshold]. *)

  let of_array_segment_one_short default a i k =
    (* [i] and [k] must represent a valid segment in the array [a]. *)
    assert (Segment.is_valid (a, i, k));
    (* This additional requirement must hold. *)
    assert (0 < k && k <= threshold);
    if k = 1 then
      S.One { default; x = Array.get a i }
    else
      S.Short { default; a = Array.sub a i k }

  let of_array_segment default a i k =
    (* [i] and [k] must represent a valid segment in the array [a]. *)
    assert (Segment.is_valid (a, i, k));
    if k = 0 then
      S.Zero { default }
    else if threshold < k then
      S.of_array_segment default a i k
    else
      of_array_segment_one_short default a i k

  let[@inline] of_array default a =
    of_array_segment default a 0 (Array.length a)

  let make default size x =
    assert (0 <= size);
    if size = 0 then
      S.Zero { default }
    else if size = 1 then
      S.One { default; x }
    else if size <= threshold then
      S.Short { default; a = Array.make size x }
    else
      S.make default size x

  let init default size f =
    assert (0 <= size);
    if size = 0 then
      S.Zero { default }
    else if size = 1 then
      S.One { default; x = f 0 }
    else if size <= threshold then
      S.Short { default; a = Array.init size f }
    else
      S.init default size f

  (* The function [S.to_array] is implemented in terms of [iter_segment]
     and therefore covers all cases. Still, it is more efficient to deal
     with each case explicitly here. *)

  let to_array s =
    match s with
    | S.Zero _ -> [||]
    | S.One { x; _ }  -> [|x|]
    | S.Short { a; _ } -> Array.copy a
    | S.Level _ -> S.to_array s

  (* [get_segment] is analogous to [to_array], but begins at index [i].
     It cannot be applied to a [Long] sequence. *)

  let get_segment pov s i =
    assert (0 <= i && i < length s);
    match s with
    | S.Zero _
    | S.Level _ ->
        assert false
    | S.One { x; _ } ->
        [|x|], 0, 1
    | S.Short { a; _ } ->
        let n = Array.length a in
        match pov with
        | Front ->
            a, i, n - i
        | Back ->
            a, 0, i + 1

  (* [of_short_array_destructive default a] is analogous to the code that is
     commented out in [wrap] (below), but expects a short array [a] instead of
     a sequence [s], and is allowed to steal this array. *)

  let of_short_array_destructive default a =
    let n = Array.length a in
    assert (n <= threshold);
    if n = 0 then
      S.Zero { default }
    else if n = 1 then
      S.One { default; x = Array.get a 0 }
    else
      S.Short { default; a }

  let[@inline] wrap_long s =
    assert (threshold < S.length s);
    s

  let wrap s =
    let n = S.length s in
    if threshold < n then
      s
    else
      let default = S.default s in
      if n = 0 then
        S.Zero { default }
      else if n = 1 then
        S.One { default; x = S.peek Front s }
      else
        let b = Array.make n default in
        let j = ref 0 in
        S.iter_segments Front s (fun (a, i, k) ->
          Array.blit a i b !j k;
          j := !j + k
        );
        let a = b in
        S.Short { default; a }

  let unwrap s =
    match s with
    | S.Zero _
    | S.Level _ ->
        s
    | S.One { default; x } ->
        S.push Back (S.create default) x
    | S.Short { default; a } ->
        S.of_array default a

  let array_push pov a x =
    let n = Array.length a in
    let b = Array.make (n+1) x in
    let i = match pov with Front -> 1 | Back -> 0 in
    Array.blit a 0 b i n;
    b

  let[@specialise] push pov s x =
    match s with
    | S.Zero { default } ->
        S.One { default; x }
    | S.One { default; x = y } ->
        let a =
          match pov with
          | Front ->
              [| x; y |]
          | Back ->
              [| y; x |]
        in
        S.Short { default; a }
    | S.Short { default; a } ->
        let n = Array.length a in
        if n < threshold then
          S.Short { default; a = array_push pov a x }
        else
          S.push pov (S.of_array default a) x
            (* equivalent to [wrap (S.push pov (unwrap s) x)] *)
    | S.Level _ ->
        S.push pov s x

  let[@specialise] pop pov s =
    match s with
    | S.Zero _ ->
        raise Empty
    | S.One { default; x } ->
        x, S.Zero { default }
    | S.Short { default; a } ->
        let n = Array.length a in
        assert (2 <= n && n <= threshold);
        begin match pov with
          | Front ->
              let x = Array.get a 0 in
              x, of_array_segment_one_short default a 1 (n-1)
          | Back ->
              let x = Array.get a (n-1) in
              x, of_array_segment_one_short default a 0 (n-1)
        end
    | S.Level _ ->
        let x, s = S.pop pov s in
        x, wrap s

  let concat s1 s2 =
    match s1, s2 with
    | S.Zero _, s
    | s, S.Zero _ ->
        s
    | S.One { x; _ }, _ ->
        push Front s2 x
    | _, S.One { x; _ } ->
        push Back s1 x
    | S.Short { default; a = a1 }, S.Short { a = a2; _ }
      when Array.length a1 + Array.length a2 <= threshold ->
        S.Short { default; a = Array.append a1 a2 }
        (* TODO: could optimize the subcase where the sum of the lengths
           exceeds [threshold] by blitting into a chunk. *)
    | (S.Short _ | S.Level _), (S.Short _ | S.Level _) ->
        let s1 = unwrap s1
        and s2 = unwrap s2 in
        S.concat s1 s2

  let sub s head size =
    assert (0 <= head && 0 <= size && head + size <= length s);
    if head = 0 && size = length s then
      s
    else if size = 0 then
      let default = S.default s in
      S.Zero { default }
    else
      match s with
      | S.Zero _
      | S.One _ ->
          (* Already dealt with above. *)
          assert false
      | S.Short { default; a } ->
          of_array_segment_one_short default a head size
      | S.Level _ ->
          if threshold < size then
            (* The desired subsequence is heavy. *)
            wrap_long (S.sub s head size)
          else
            (* The desired subsequence is short. We do not want to allocate
               a heavy sequence as an intermediate step, only to convert it
               into a short sequence immediately afterwards. *)
            let default = S.default s in
            if size = 1 then
              (* Here, a single call to [S.get] suffices. *)
              S.One { default; x = S.get s head }
            else begin
              (* Here, we use [Array.init], combined with an iterator. *)
              let it = Iter.create front s in
              Iter.reach it head;
              let get _i =
                let x = Iter.get it in
                Iter.move front it;
                x
              in
              let a = Array.init size get in
              S.Short { default; a }
            end

  let split s i =
    let n = length s in
    assert (0 <= i && i <= n);
    if i <= threshold || n - i <= threshold then
      (* We recognize the special cases where one of the two subsequences
         is short, and use [sub] in that case, so as to avoid creating a
         heavy sequence as an intermediate step. *)
      sub s 0 i, sub s i (n - i)
        (* TODO by calling [sub] twice, we may be creating two iterators;
           we could use only one and save one call to [reach]. *)
    else
      match s with
      | S.Zero _
      | S.One _
      | S.Short _ ->
          (* Already dealt with above. *)
          assert false
      | S.Level _ ->
          let s1, s2 = S.split s i in
          wrap_long s1, wrap_long s2

  let take s i =
    assert (0 <= i && i <= length s);
    sub s 0 i

  let drop s i =
    assert (0 <= i && i <= length s);
    sub s i (length s - i)

  let get s i =
    assert (0 <= i && i < length s);
    match s with
    | S.One { x; _ } ->
        x
    | S.Short { a; _ } ->
        Array.get a i
    | S.Zero _
    | S.Level _ ->
        S.get s i

  let set s i x' =
    assert (0 <= i && i < length s);
    match s with
    | S.One { x; default } ->
        if x == x' then s else
        let x = x' in
        S.One { x; default }
    | S.Short { a; default } ->
        let x = Array.get a i in
        if x == x' then s else
        let a = Array.copy a in
        Array.set a i x';
        S.Short { a; default }
    | S.Zero _
    | S.Level _ ->
        S.set s i x'

end (* Make *)
