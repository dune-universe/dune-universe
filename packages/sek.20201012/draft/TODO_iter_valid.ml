

(* ESek:

   Every iterator has a version number: [it.version], greater than zero.

   Each sequence has a field [iterator_version] with the following specification:
   - an iterator is usable iff [it.version = it.seq.iterator_version].
   - all iterators associated with the current sequence have version number no greater than
     [abs (seq.iterator_version)]
   - if [seq.iterator_version > 0], then there exists at least one iterator on the current sequence
   - [seq.iterator_version <= 0] indicates that there is no usable iterator on the sequence.
*)

type 'a ESek.t = {
  ...;
  iterator_version : int;
  }

let invalidate_all_iterators s =
  if s.iterator_version > 0
    then s.iterator_version <- (- s.iterator_version)

let get_version_for_iterator s =
  (* if no active iterator, get next version number, else use current number *)
  if s.iterator_version <= 0
    then s.iterator_version <- (- s.iterator_version) + 1;
  s.iterator_version

(* Operations that modify the structures do invalidate all iterators.
   (During the lifetime of a sequence, there is at most one extra write
   performed for iterator invalidation per iterator created). *)

let push s x =
  invalidate_all_iterators s; (* added *)
  ...



(* in Iter, creation of an iterator obtains a version number *)

type 'a ESek.Iter.t = {
  ...;
  version : int;
  }

let create s =
  { seq = s;
    ...
    version = get_version_for_iterator s; } (* added *)



(*******************************************************************************)

(* Other feature : optimization of [get] and [set] for sequences, ephemeral
   sequences and possibly also persistent sequences, by keeping track of
   a "canonical" iterator associated with the current sequence. *)

type 'a ESek.t = {
  ...;
  current_it : ('a ESek.Iter.t) option;
  }

let invalidate_all_iterators s =
  if s.current_it <> None
    then s.current_it <- None;
  ... (* same as above in this file *)

let ensure_exists_current_it s =
  if s.current_it = None
    then s.current_it <- Some (Iter.create s);

let reach_with_current_it s i =
  ensure_exists_current_it s;
  let it = unsome s.current_it in
  Iter.reach s.current_it i

let get s i =
  if not (0 <= i && i < length s)
    then raise Invalid_arg;
  reach_with_current_it s i;
  Iter.unsafe_get (unsome it.current_it)

let set s i v =
  if not (0 <= i && i < length s)
    then raise Invalid_arg;
  if is_ownership_full s then begin (* see TODO_full_ownership *)
    reach_with_current_it s i;
    Iter.unsafe_set (unsome it.current_it) v
  end else
    previous_implementation_of_set s i v
    (* Note: could be optimized if the set reaches a uniquely owned
       schunk, but this would complicate things. *)

(* It might be useful to let user exploit the iterator associated
   with the sequence *)

let get_iterator s =
  ensure_exists_current_it s;
  unsome s.current_it
