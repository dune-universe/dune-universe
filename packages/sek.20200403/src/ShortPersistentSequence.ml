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
    (T : THRESHOLD)
= struct

  open T

  (* We define the following types of sequences:

     * [Zero] represents an empty sequence.

     * [One] represents a singleton sequence.

     * [Short] represents a sequence whose length is comprised between
       2 and [threshold], included.

     * [Long] represents a sequence whose length is greater than [threshold].

     The constructors [Zero], [One], and [Short] carry a default value. *)

  (* I believe that this code works even if [threshold] is less than 2.
     In that case, the [Short] representation is simply never used. *)

  type 'a t =
    | Zero of { default: 'a }
    | One   of { default: 'a; x: 'a }
    | Short of { default: 'a; a: 'a array }
    | Long  of 'a S.t

  let default s =
    match s with
    | Zero { default }
    | One { default; _ }
    | Short { default; _ } ->
        default
    | Long s ->
        S.default s

  let length s =
    match s with
    | Zero _ ->
        0
    | One _ ->
        1
    | Short { a; _ } ->
        Array.length a
    | Long s ->
        S.length s

  let is_empty s =
    match s with
    | Zero _ ->
        true
    | One _
    | Short _
    | Long _ ->
        (* A [Long] sequence must be nonempty. *)
        false

  let check s =
    match s with
    | Zero _
    | One _ ->
        ()
    | Short { a; _ } ->
        let n = Array.length a in
        assert (2 <= n && n <= threshold)
    | Long s ->
        let n = S.length s in
        assert (n > threshold);
        S.check s

  (* Ensure [check] has zero cost in release mode. *)

  let[@inline] check s =
    assert (check s; true)

  let print print s =
    let open PPrint in
    let open PPrint.OCaml in
    match s with
    | Zero _ ->
        !^ "Zero"
    | One { x; _ } ->
        !^ "One" ^^ record "pseq" [
          "model", flowing_list print [x]
        ]
    | Short { a; _ } ->
        !^ "Short" ^^ record "pseq" [
          "model", flowing_list print (Array.to_list a)
        ]
    | Long s ->
        variant "pseq" "Long" 3 [ S.print print s ]

  let create default =
    Zero { default }

  (* [of_array_segment_one_short] is a special case of [of_array_segment]
     with the additional precondition [0 < k && k <= threshold]. *)

  let of_array_segment_one_short default a i k =
    (* [i] and [k] must represent a valid range in the array [a]. *)
    assert (ArrayExtra.is_valid_segment a i k);
    (* This additional requirement must hold. *)
    assert (0 < k && k <= threshold);
    if k = 1 then
      One { default; x = a.(i) }
    else
      Short { default; a = Array.sub a i k }

  let of_array_segment default a i k =
    (* [i] and [k] must represent a valid range in the array [a]. *)
    assert (ArrayExtra.is_valid_segment a i k);
    if k = 0 then
      Zero { default }
    else if threshold < k then
      Long (S.of_array_segment default a i k)
    else
      of_array_segment_one_short default a i k

  let[@inline] of_array default a =
    of_array_segment default a 0 (Array.length a)

  let make default size x =
    assert (0 <= size);
    if size = 0 then
      Zero { default }
    else if size = 1 then
      One { default; x }
    else if size <= threshold then
      Short { default; a = Array.make size x }
    else
      Long (S.make default size x)

  let init default size f =
    assert (0 <= size);
    if size = 0 then
      Zero { default }
    else if size = 1 then
      One { default; x = f 0 }
    else if size <= threshold then
      Short { default; a = Array.init size f }
    else
      Long (S.init default size f)

  let to_array s =
    match s with
    | Zero _ -> [||]
    | One { x }  -> [|x|]
    | Short { a; _ } -> Array.copy a
    | Long s -> S.to_array s

  (* [of_short_array_destructive default a] is analogous to the code that is
     commented out in [wrap] (below), but expects a short array [a] instead of
     a sequence [s], and is allowed to steal this array. *)

  let of_short_array_destructive default a =
    let n = Array.length a in
    assert (n <= threshold);
    if n = 0 then
      Zero { default }
    else if n = 1 then
      One { default; x = a.(0) }
    else
      Short { default; a }

  let wrap_long s =
    assert (threshold < S.length s);
    Long s

  let wrap s =
    let n = S.length s in
    if threshold < n then
      Long s
    else
      let default = S.default s in
      if n = 0 then
        Zero { default }
      else if n = 1 then
        One { default; x = S.peek Front s }
      else
        let a = Array.make n default in
        let i = ref 0 in
        S.iter Front (fun x -> a.(!i) <- x; i := !i + 1) s;
        Short { default; a }

  let unwrap s =
    match s with
    | Zero { default } ->
        S.create default
    | One { default; x } ->
        S.push Back (S.create default) x
    | Short { default; a } ->
        S.of_array default a
    | Long s ->
        s

  let array_push pov a x =
    let n = Array.length a in
    let b = Array.make (n+1) x in
    let i = match pov with Front -> 1 | Back -> 0 in
    Array.blit a 0 b i n;
    b

  let[@specialise] push pov s x =
    match s with
    | Zero { default } ->
        One { default; x }
    | One { default; x = y } ->
        let a =
          match pov with
          | Front ->
              [| x; y |]
          | Back ->
              [| y; x |]
        in
        Short { default; a }
    | Short { default; a } ->
        let n = Array.length a in
        if n < threshold then
          Short { default; a = array_push pov a x }
        else
          Long (S.push pov (S.of_array default a) x)
            (* equivalent to [wrap (S.push pov (unwrap s) x)] *)
    | Long s ->
        Long (S.push pov s x)

  let[@specialise] pop pov s =
    match s with
    | Zero _ ->
        raise Empty
    | One { default; x } ->
        x, Zero { default }
    | Short { default; a } ->
        let n = Array.length a in
        assert (2 <= n && n <= threshold);
        begin match pov with
          | Front ->
              let x = a.(0) in
              x, of_array_segment_one_short default a 1 (n-1)
          | Back ->
              let x = a.(n-1) in
              x, of_array_segment_one_short default a 0 (n-1)
        end
    | Long s ->
        let x, s = S.pop pov s in
        x, wrap s

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
            a.(0)
        | Back ->
            let n = Array.length a in
            a.(n-1)
        end
    | Long s ->
        S.peek pov s

  let[@specialise] array_iter pov f a =
    match pov with
    | Front ->
        Array.iter f a
    | Back ->
        let n = Array.length a in
        for i = n - 1 downto 0 do
          f a.(i)
        done

  let[@specialise] iter pov g s =
    match s with
    | Zero _ ->
        ()
    | One { x; _ } ->
        g x
    | Short { a; _ } ->
        array_iter pov g a
    | Long s ->
        S.iter pov g s

  let concat s1 s2 =
    match s1, s2 with
    | Zero _, s
    | s, Zero _ ->
        s
    | One { x; _ }, _ ->
        push Front s2 x
    | _, One { x; _ } ->
        push Back s1 x
    | Short { default; a = a1 }, Short { a = a2; _ }
      when Array.length a1 + Array.length a2 <= threshold ->
        Short { default; a = Array.append a1 a2 }
        (* TODO: could optimize the subcase where the sum of the lengths
           exceeds [threshold] by blitting into a chunk. *)
    | (Short _ | Long _), (Short _ | Long _) ->
        let s1 = unwrap s1
        and s2 = unwrap s2 in
        Long (S.concat s1 s2)

  let split s i =
    assert (0 <= i && i <= length s);
    if i = 0 then
      let default = default s in
      Zero { default }, s
    else if i = length s then
      let default = default s in
      s, Zero { default }
    else
      match s with
      | Zero _
      | One _ ->
          (* Already dealt with above. *)
          assert false
      | Short { default; a } ->
          let n = Array.length a in
          of_array_segment_one_short default a 0 i,
          of_array_segment_one_short default a i (n-i)
      | Long s ->
          let s1, s2 = S.split s i in
          wrap s1, wrap s2

  let get s i =
    assert (0 <= i && i < length s);
    match s with
    | Zero _ ->
        assert false
    | One { x; _ } ->
        x
    | Short { a; _ } ->
        Array.get a i
    | Long s ->
        S.get s i

  let set s i x' =
    assert (0 <= i && i < length s);
    match s with
    | Zero _ ->
        assert false
    | One { x; default } ->
        if x == x' then s else
        let x = x' in
        One { x; default }
    | Short { a; default } ->
        let x = Array.get a i in
        if x == x' then s else
        let a = Array.copy a in
        Array.set a i x';
        Short { a; default }
    | Long s as original ->
        let s' = S.set s i x' in
        if s == s' then original else
        let s = s' in
        Long s

end
