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

open Printf

(* -------------------------------------------------------------------------- *)

(* The reference implementation is parameterized over the following concrete
   types. *)

module Make (X : sig

  type side
  val front: side
  (* val back: side *)
  val other: side -> side

  type direction
  val forward: direction
  (* val backward: direction *)
  (* val sign: direction -> int *)

  exception Empty
  exception End

end) = struct

open X

(* -------------------------------------------------------------------------- *)

(* The reference implementation of iterators. *)

module Iter (S : sig

  type 'a t

  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit

  (* The sequence [s]; the length [n] of the sequence, which must not change
     as long as the iterator remains valid; and the current index [i]. The
     flag [v] indicates whether the iterator is currently valid. *)

  type 'a iter = {
    mutable s : 'a t;
    mutable n : int;
    mutable i : int;
    mutable v: bool;
  }

  (* [declare s it] registers the existence of a new iterator [it] for the
     sequence [s]. *)

  val declare : 'a t -> 'a iter -> 'a iter

end) = struct

  type 'a iter = 'a S.iter = {
    mutable s : 'a S.t;
    mutable n : int;
    mutable i : int;
    mutable v: bool;
  }

  let create_at_sentinel direction s =
    let n = S.length s in
    let i = if direction = forward then -1 else n in
    let v = true in
    S.declare s { s; n; i; v }

  let reset_at_sentinel direction it =
    (* The length must be read again,
       as it may have changed; [reset]
       can be applied to an invalidated iterator. *)
    let n = S.length it.s in
    it.n <- n;
    it.i <- (if direction = forward then -1 else n);
    it.v <- true

  let copy it =
    S.declare it.s { it with i = it.i }

  let sequence it =
    it.s

  let length it =
    it.n

  let index it =
    it.i

  let finished it =
    it.i = -1 || it.i = it.n

  let get it =
    if finished it then
      raise End
    else
      S.get it.s it.i

  let get_opt it =
    if finished it then
      None
    else
      Some (S.get it.s it.i)

  let reach it j =
    it.i <- j

  let jump direction it k =
    if direction = forward then begin
      it.i <- it.i + k;
      assert (it.i <= it.n)
    end
    else begin
      it.i <- it.i - k;
      assert (-1 <= it.i)
    end

  let move direction it =
    jump direction it 1

  let get_and_move direction it =
    let x = get it in
    move direction it;
    x

  let get_and_move_opt direction it =
    if finished it then
      None
    else
      Some (get_and_move direction it)

  let create direction s =
    let it = create_at_sentinel direction s in
    move direction it;
    it

  let reset direction it =
    reset_at_sentinel direction it;
    move direction it

  let check _it = ()

  let is_valid it =
    it.v

  let set it x =
    if finished it then
      raise End
    else begin
      S.set it.s it.i x;
      it.v <- true (* this iterator remains valid *)
    end

  let set_and_move dir it x =
    set it x;
    move dir it

  open Monolith
  open Monolith.Print
  let (!^) = PPrint.(!^)
  let (^^) = PPrint.(^^)
  let (^/^) = PPrint.(^/^)
  let utf8format = PPrint.utf8format
  let apply_parens doc docs = apply doc (List.map parens docs)
  let not_ doc = apply_parens (!^ "not") doc
  let assert_not_ doc = assert_ (not_ [doc])

  (* This function determines whether an array segment is well-formed. *)

  let is_valid_array_segment (a, i, k) =
    let n = Array.length a in
    if 0 <= k && 0 <= i && i + k <= n then
      Valid
    else
      Invalid (fun o -> assert_ (apply (!^ "Segment.is_valid") [o]))

  (* This function determines whether an array segment is nonempty. *)

  let is_nonempty_array_segment (_, _, k) =
    if 0 < k then
      Valid
    else
      Invalid (fun o -> assert_not_ (apply (!^ "Segment.is_empty") [o]))

  (* [some_diagnostic] adapts a diagnostic so that it refers to [Some x]
     instead of [x]. *)

  let some_diagnostic = function
    | Valid ->
        Valid
    | Invalid assertion ->
        Invalid (fun x ->
          !^ "let Some " ^^ x ^^
          !^ " = " ^^ x ^^
          !^ " in" ^/^
          assertion x)

  (* This printer produces a string representation of a valid array segment.
     It is used only inside an OCaml comment, so it does not have to be
     well-formed OCaml code. *)

  let print_array_segment print_element (a, i, k) =
    let xs = Array.sub a i k in
    "[| ... " ^
    Array.fold_right (fun x s -> sprintf "%s;%s" (print_element x) s) xs " ... |]"

  (* [array_rev dir a] produces a copy of the array [a], which is reversed if
     [dir] is [backward]. *)

  let array_rev dir a =
    if dir = forward then
      a
    else
      let n = Array.length a in
      Array.init n (fun i -> a.(n - 1 - i))

  (* [get_segment] has a nondeterministic specification. Its reference
     implementation has access to the array segment [seg] produced by the
     candidate implementation and must determine whether it is valid. *)

  let get_segment dir it =
    if finished it then raise End else fun seg ->
    match is_valid_array_segment seg with
    | Invalid _ as diagnostic ->
        diagnostic
    | Valid ->
    match is_nonempty_array_segment seg with
    | Invalid _ as diagnostic ->
        diagnostic
    | Valid ->
        (* The candidate returns a well-formed array segment. *)
        (* Convert this array segment to an array [elements] of pairs of
           an index (into the array [a]) and an element, while respecting
           the direction [dir], and check that the elements that lie in
           the sequence ahead of the reference iterator [it] match this
           list of elements. To do this, it's convenient to create our
           local copy of [it]. *)
        let (a, i, k) = seg in
        let elements = Array.init (Array.length a) (fun i -> (i, a.(i))) in
        let elements = Array.sub elements i k in
        let elements = array_rev dir elements in
        let it' = { it with i = it.i } in (* our local copy of [it] *)
        (* for progress = 0 to k - 1 do: *)
        let rec loop progress =
          if progress = k then
            Valid
          else
            let index, ce = elements.(progress) in
            match get_and_move_opt dir it' with
            | None ->
                (* The reference iterator runs out of elements, yet the
                   segment produced by the candidate is not finished. *)
                Invalid (fun x ->
                  !^ "let Some (_a, _i, k) = " ^^ x ^^
                  !^ " in" ^/^
                  assert_ (utf8format "k <= %d" progress) ^^
                  !^ ";;" ^^
                  candidate_finds (int k)
                )
            | Some re ->
                if re <> ce then begin
                  (* The reference iterator produces [re], while the segment
                     produced by the candidate iterator contains [ce]. *)
                  Invalid (fun x ->
                    !^ "let Some (a, _i, _k) = " ^^ x ^^
                    !^ " in" ^/^
                    assert_ (utf8format "a.(%d) = %d" index re) ^^
                    !^ ";;" ^^
                    candidate_finds (int ce)
                  )
                end
                else
                  loop (progress + 1)
        in
        loop 0

  (* [get_segment_opt] has a nondeterministic specification. Its reference
     implementation has access to the optional array segment [oseg]
     produced by the candidate implementation and must determine whether it is
     valid. *)

  let get_segment_opt dir it oseg =
    match oseg with
    | None ->
        (* The candidate claims that this iterator is finished. *)
        begin match get_opt it with
        | None ->
            (* It is indeed finished. *)
            Valid
        | Some _ ->
            (* It is not finished. *)
            Invalid (fun x ->
              assert_ (x ^^ !^ " <> None") ^^
              !^ ";;" ^^
              candidate_finds (!^ "None")
            )
        end
    | Some seg ->
        some_diagnostic (get_segment dir it seg)

  let get_segment_and_jump dir it =
    if finished it then raise End else fun seg ->
    let diagnostic = get_segment dir it seg in
    match diagnostic, seg with
    | Valid, (_, _, k) ->
        jump dir it k;
        Valid
    | Invalid _, _ ->
        diagnostic

  let get_segment_and_jump_opt dir it oseg =
    let diagnostic = get_segment_opt dir it oseg in
    match diagnostic, oseg with
    | Valid, Some (_, _, k) ->
        jump dir it k;
        Valid
    | Valid, None
    | Invalid _, _ ->
        diagnostic

end (* Iter *)

(* -------------------------------------------------------------------------- *)

(* The reference implementation of persistent sequences. *)

module Persistent = struct

  (* Replace OCaml's default implementation of list concatenation with
     a tail recursive implementation. This should decrease our chances
     of blowing the stack. *)

  let (@) xs ys =
    List.rev_append (List.rev xs) ys

  (* Persistent sequences are implemented by lists. *)

  type 'a t = 'a list

  type 'a iter = {
    mutable s : 'a t;
    mutable n : int;
    mutable i : int;
    mutable v: bool;
  }

  let create (_default : 'a) : 'a list = []

  let rec repeat n x xs =
    if n = 0 then
      xs
    else
      repeat (n - 1) x (x :: xs)

  let repeat n x =
    repeat n x []

  let make _ n x =
    repeat n x

  let rec init i j f xs =
    if i < j then
      init (i + 1) j f (f i :: xs)
    else
      xs

  let init n f =
    List.rev (init 0 n f [])

  let init _ n f =
    init n f

  let length = List.length

  let is_empty xs =
    xs = []

  let push_front s x = x :: s

  let push_back s x = s @ [x]

  let push side s x =
    if side = front then push_front s x else push_back s x

  let pop_front_opt s =
    match s with
    | [] ->
        None, []
    | x :: xs ->
        Some x, xs

  let pop_back_opt s =
    let ox, s = pop_front_opt (List.rev s) in
    ox, List.rev s

  let pop_opt side s =
    if side = front then pop_front_opt s else pop_back_opt s

  let pop side s =
    let ox, s = pop_opt side s in
    match ox with
    | None ->
        raise Empty
    | Some x ->
        x, s

  let peek_front_opt s =
    match s with
    | [] ->
        None
    | x :: _ ->
        Some x

  let rec last1 x xs =
    match xs with
    | [] ->
        Some x
    | x :: xs ->
        last1 x xs

  let last xs =
    match xs with
    | [] ->
        None
    | x :: xs ->
        last1 x xs

  let peek_back_opt =
    last

  let peek_opt side s =
    if side = front then peek_front_opt s else peek_back_opt s

  let peek side s =
    match peek_opt side s with
    | Some x ->
        x
    | None ->
        raise Empty

  let get =
    List.nth

  let rec set xs i x' =
    match xs, i with
    | [], _ ->
        assert false (* invalid argument *)
    | _x :: xs, 0 ->
        x' :: xs
    | x :: xs, _ ->
        x :: set xs (i - 1) x'

  let concat = (@)

  let rec split xs i =
    if i = 0 then
      [], xs
    else
      match xs with
      | [] ->
          assert false (* invalid argument *)
      | x :: xs ->
          let xs, ys = split xs (i - 1) in
          x :: xs, ys

  let rec take xs i =
    if i = 0 then
      []
    else
      match xs with
      | [] ->
          assert false (* invalid argument *)
      | x :: xs ->
          let xs = take xs (i - 1) in
          x :: xs

  let of_list_segment _d n xs =
    take xs n

  let rec drop xs i =
    if i = 0 then
      xs
    else
      match xs with
      | [] ->
          assert false (* invalid argument *)
      | _ :: xs ->
          drop xs (i - 1)

  let sub xs i k =
    take (drop xs i) k

  (* Unused:
  let rec fill xs i k x =
    if i = 0 then
      if k = 0 then
        xs
      else
        x :: fill (List.tl xs) i (k - 1) x
    else
      (List.hd xs) :: fill (List.tl xs) (i - 1) k x
   *)

  let fill s i k x =
    let a = Array.of_list s in
    Array.fill a i k x;
    Array.to_list a

  let blit xs i ys j k =
    (* Hmmm... let's do it the easy way. *)
    let xs = Array.of_list xs
    and ys = Array.of_list ys in
    Array.blit xs i ys j k;
    Array.to_list ys

  let take side xs i =
    if side = front then take xs i else drop xs i

  let drop side xs i =
    take (other side) xs i

  let iter direction f xs =
    if direction = forward then
      List.iter f xs
    else
      List.iter f (List.rev xs)

  let iteri direction f xs =
    if direction = forward then
      List.iteri f xs
    else
      let n = List.length xs in
      let opposite i = n - 1 - i in
      List.iteri (fun i x -> f (opposite i) x) (List.rev xs)

  let iter2 direction f xs ys =
    let xs = Array.of_list xs
    and ys = Array.of_list ys in
    let n1 = Array.length xs
    and n2 = Array.length ys in
    let n = min n1 n2 in
    if direction = forward then
      let xs = Array.sub xs 0 n
      and ys = Array.sub ys 0 n in
      Array.iter2 f xs ys
    else
      let xs = Array.sub xs (n1 - n) n
      and ys = Array.sub ys (n2 - n) n in
      for i = n - 1 downto 0 do
        f xs.(i) ys.(i)
      done

  let of_array _ a =
    Array.to_list a

  let of_array_segment d a i k =
    of_array d (Array.sub a i k)

  let rec of_seq_segment d n xs =
    if n = 0 then
      []
    else
      match xs() with
      | Seq.Nil ->
          assert false
      | Seq.Cons (x, xs) ->
          x :: of_seq_segment d (n - 1) xs

  let of_seq _d xs =
    List.of_seq xs

  let to_array =
    Array.of_list

  let to_list xs = xs

  let of_list _d xs = xs

  let rev direction xs =
    if direction = forward then xs else List.rev xs

  let to_seq direction xs =
    List.to_seq (rev direction xs)

  let find_opt direction p xs =
    List.find_opt p (rev direction xs)

  (* From OCaml 4.10.0. *)
  let rec find_map f = function
    | [] -> None
    | x :: l ->
       begin match f x with
         | Some _ as result -> result
         | None -> find_map f l
       end

  let find_map direction p xs =
    find_map p (rev direction xs)

  let for_all =
    List.for_all

  let exists =
    List.exists

  let mem =
    List.mem

  let map _d f s =
    List.map f s

  let mapi _d f s =
    List.mapi f s

  let rev =
    List.rev

  let filter_map _d f xs =
    List.filter_map f xs

  let partition =
    List.partition

  let flatten_map _d f xs =
    List.flatten (List.map f xs)

  let equalize s1 s2 =
    let n1 = List.length s1
    and n2 = List.length s2 in
    if n1 <= n2 then
      s1, take front s2 n1
    else
      take front s1 n2, s2

  let map2 _d f s1 s2 =
    let s1, s2 = equalize s1 s2 in
    List.map2 f s1 s2

  let for_all2 p s1 s2 =
    let s1, s2 = equalize s1 s2 in
    List.for_all2 p s1 s2

  let exists2 p s1 s2 =
    let s1, s2 = equalize s1 s2 in
    List.exists2 p s1 s2

  let rec equal eq xs ys =
    match xs, ys with
    | [], [] ->
        true
    | [], _ :: _
    | _ :: _, [] ->
        false
    | x :: xs, y :: ys ->
        eq x y && equal eq xs ys

  let rec compare cmp xs ys =
    match xs, ys with
    | [], [] ->
        0
    | [], _ :: _ ->
        -1
    | _ :: _, [] ->
        1
    | x :: xs, y :: ys ->
        let c = cmp x y in
        if c <> 0 then c else compare cmp xs ys

  let sort =
    List.sort

  let stable_sort =
    List.stable_sort

  let rec uniq1 cmp x ys =
    match ys with
    | [] ->
        []
    | y :: ys ->
        if cmp x y = 0 then
          uniq1 cmp x ys
        else
          y :: uniq1 cmp y ys

  let uniq cmp xs =
    match xs with
    | [] ->
        []
    | x :: xs ->
        x :: uniq1 cmp x xs

  let merge =
    List.merge

  let filter = List.filter

  let check _s = ()

  module Iter =
    Iter(struct
      type nonrec 'a t = 'a t
      type nonrec 'a iter = 'a iter = {
        mutable s : 'a t;
        mutable n : int;
        mutable i : int;
        mutable v: bool;
      }
      let length = length
      let get = get
      let set _s _i _x = assert false
      let declare _s it = it
    end)

end (* Persistent *)

(* -------------------------------------------------------------------------- *)

(* The reference implementation of ephemeral sequences. *)

module Ephemeral = struct

  module P = Persistent

  (* The reference implementation must be able to tell whether an iterator is
     valid. We implement this in the most straightforward way by letting each
     sequence keep a list of its iterators. This creates a mutual recursion
     between the types [t] and [iter], which is why the functor [Iter] must
     take the type [iter] as an argument, instead of defining it internally. *)

  type 'a t =
    { mutable seq : 'a P.t;
      mutable iterators : 'a iter list }

  and 'a iter = {
    mutable s : 'a t;
    mutable n : int;
    mutable i : int;
    mutable v: bool;
  }

  (* Internal operations. *)

  let fresh (seq : 'a P.t) : 'a t =
    { seq; iterators = [] }

  let seq (s : 'a t) : 'a P.t =
    s.seq

  let invalidate s =
    List.iter (fun it -> it.v <- false) s.iterators

  let write s seq =
    invalidate s;
    s.seq <- seq

  (* Public operations. *)

  let create default =
    fresh (P.create default)

  let make d n x =
    fresh (P.make d n x)

  let init d n f =
    fresh (P.init d n f)

  let length s =
    P.length (seq s)

  let is_empty s =
    P.is_empty (seq s)

  let clear s =
    write s []

  let copy ?mode:(_mode = `Copy) s =
    fresh (seq s)

  let assign s1 s2 =
    if s1 != s2 then begin
      write s1 (seq s2);
      clear s2
    end

  let push side s x =
    write s (P.push side (seq s) x)

  let pop_opt side s =
    let ox, xs = P.pop_opt side (seq s) in
    write s xs;
    ox

  let pop side s =
    match pop_opt side s with
    | None ->
        raise Empty
    | Some x ->
        x

  let peek_opt side s =
    P.peek_opt side (seq s)

  let peek side s =
    match peek_opt side s with
    | None ->
        raise Empty
    | Some x ->
        x

  let get s =
    P.get (seq s)

  let set s i x =
    write s (P.set (seq s) i x)

  let concat s1 s2 =
    let s = fresh (P.concat (seq s1) (seq s2)) in
    clear s1;
    clear s2;
    s

  let append_back s1 s2 =
    (* Move data from [s2] into the back of [s1]. Clear [s2]. *)
    write s1 (P.concat (seq s1) (seq s2));
    clear s2

  let append_front s1 s2 =
    (* Move data from [s2] into the front of [s1]. Clear [s2]. *)
    write s1 (P.concat (seq s2) (seq s1));
    clear s2

  let append side s1 s2 =
    if side = front then
      append_front s1 s2
    else
      append_back s1 s2

  let split s i =
    let xs, ys = P.split (seq s) i in
    clear s;
    fresh xs, fresh ys

  let carve_front s i =
    (* Extract the [i] front elements in a new sequence. *)
    let xs, ys = P.split (seq s) i in
    write s ys;
    fresh xs

  let carve_back s i =
    (* Keep the [i] front elements and extract the rest in a new sequence. *)
    let xs, ys = P.split (seq s) i in
    write s xs;
    fresh ys

  let take side s i =
    write s (P.take side (seq s) i)

  let drop side s i =
    take (other side) s i

  let sub s i k =
    fresh (P.sub (seq s) i k)

  let fill s i k x =
    write s (P.fill (seq s) i k x)

  let blit xs i ys j k =
    write ys (P.blit (seq xs) i (seq ys) j k)

  let carve side s i =
    if side = front then
      carve_front s i
    else
      carve_back s i

  let iter direction f s =
    P.iter direction f (seq s)

  let iteri direction f s =
    P.iteri direction f (seq s)

  let iter2 direction f s1 s2 =
    P.iter2 direction f (seq s1) (seq s2)

  let of_list_segment d n xs =
    fresh (P.of_list_segment d n xs)

  let of_array d a =
    fresh (P.of_array d a)

  let of_array_segment d a i k =
    fresh (P.of_array_segment d a i k)

  let of_seq_segment d n xs =
    fresh (P.of_seq_segment d n xs)

  let of_seq d xs =
    fresh (P.of_seq d xs)

  let to_array s =
    P.to_array (seq s)

  let to_list s =
    P.to_list (seq s)

  let of_list d xs =
    fresh (P.of_list d xs)

  let to_seq direction s =
    P.to_seq direction (seq s)

  let find_opt direction p s =
    P.find_opt direction p (seq s)

  let find_map direction p s =
    P.find_map direction p (seq s)

  let for_all p s =
    P.for_all p (seq s)

  let exists p s =
    P.exists p (seq s)

  let mem x s =
    P.mem x (seq s)

  let map d f s =
    fresh (P.map d f (seq s))

  let mapi d f s =
    fresh (P.mapi d f (seq s))

  let rev s =
    fresh (P.rev (seq s))

  let filter_map d p s =
    fresh (P.filter_map d p (seq s))

  let partition p s =
    let s1, s2 = P.partition p (seq s) in
    fresh s1, fresh s2

  let flatten_map d f s =
    fresh (P.flatten_map d (fun x -> (seq (f x))) (seq s))

  let map2 d f s1 s2 =
    fresh (P.map2 d f (seq s1) (seq s2))

  let for_all2 f s1 s2 =
    P.for_all2 f (seq s1) (seq s2)

  let exists2 f s1 s2 =
    P.exists2 f (seq s1) (seq s2)

  let equal eq s1 s2 =
    P.equal eq (seq s1) (seq s2)

  let compare cmp s1 s2 =
    P.compare cmp (seq s1) (seq s2)

  let sort cmp s =
    write s (P.sort cmp (seq s))

  let stable_sort cmp s =
    write s (P.stable_sort cmp (seq s))

  let uniq cmp s =
    fresh (P.uniq cmp (seq s))

  let merge cmp s1 s2 =
    fresh (P.merge cmp (seq s1) (seq s2))

  let filter p s =
    fresh (P.filter p (seq s))

  let check _s = ()

  module Iter = struct

    include Iter(struct
      type nonrec 'a t = 'a t
      type nonrec 'a iter = 'a iter = {
        mutable s : 'a t;
        mutable n : int;
        mutable i : int;
        mutable v: bool;
      }
      let length = length
      let get = get
      let set = set
      let declare s it =
        s.iterators <- it :: s.iterators;
        it
    end)

  end

end (* Ephemeral *)

(* -------------------------------------------------------------------------- *)

(* The reference implementation of the conversion functions. *)

module E = Ephemeral
module P = Persistent

open E

let snapshot s =
  seq s

let edit xs =
  fresh xs

let snapshot_and_clear s =
  let xs = snapshot s in
  clear s;
  xs

(* -------------------------------------------------------------------------- *)

end (* Make *)
