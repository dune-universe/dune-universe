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

let (!^) = PPrint.(!^)
let foreach xs f = List.iter f xs
open Monolith
open Prologue
open Selection

module Prologue () = struct

(* -------------------------------------------------------------------------- *)

(* Choose parameters with which to instantiate [Sek]. *)

(* At the same time, we print our parameters to the standard output channel. *)

module Settings = struct

  open Printf
  open Gen

  let () =
    printf "module Settings = struct\n"

  (* We do not want to fail in this setup phase when we run out of data, so
     we define these modified generators. *)

  let interval_ i j () =
    try interval_ i j () with End_of_file -> i

  let bool () =
    try bool() with End_of_file -> true

  (* Pick a nonzero chunk size at each depth. We allow different chunk sizes
     at depths 0, 1, and 2, then impose a uniform chunk size, for simplicity.
     Most tests are shallow anyway. *)

  let capacity0, capacity1, capacity2 =
    interval_ 2 16 (),
    interval_ 2 4 (),
    interval_ 2 4 ()

  let capacity depth =
    match depth with
    | 0 -> capacity0
    | 1 -> capacity1
    | _ -> capacity2

  let () =
    printf
      "  let capacity = function 0 -> %d | 1 -> %d | _ -> %d\n"
            capacity0 capacity1 capacity2

  (* Decide whether empty slots should be overwritten with a dummy value. *)

  let overwrite_empty_slots =
    bool()

  let () =
    printf "  let overwrite_empty_slots = %b\n" overwrite_empty_slots

  (* Pick the threshold up to which we represent a persistent sequence as an
     immutable array. *)

  let threshold =
    interval_ 2 32 ()

  let () =
    printf "  let threshold = %d\n" threshold

  (* Decide whether iterator validity should be checked. *)

  let check_iterator_validity =
    bool()

  let () =
    printf "  let check_iterator_validity = %b\n" check_iterator_validity

  let () =
    printf "end\n"

  let () =
    (* Print a functor invocation.. *)
    printf "module C = Sek.Make(Settings)\n";
    printf "open C;;\n";
    (* Install convenient printers for debugging in the OCaml toplevel loop. *)
    printf "#install_printer E.format;;\n";
    printf "#install_printer E.Iter.format;;\n";
    printf "#install_printer P.format;;\n";
    printf "#install_printer P.Iter.format;;\n";
    (* Load this file, which contains some of the code that is needed to
       reproduce a problematic scenario. *)
    printf "#use \"monolith/src/Prologue.ml\";;\n";
    ()

end

(* -------------------------------------------------------------------------- *)

(* Name our reference implementation and our candidate implementation. *)

module C = Sek.Make(Settings)
module R = Reference.Make(C)

(* -------------------------------------------------------------------------- *)

(* Define [element] as an alias for the concrete type [int]. Equip it with a
   deterministic generator of fresh elements. There is no point in letting
   afl-fuzz choose elements in a nondeterministic way; that would be a waste
   of random bits. *)

let next =
  Gen.sequential()

let element =
  int &&& next

(* -------------------------------------------------------------------------- *)

(* Define [index] as an alias for [int]. *)

(* This type is used as output only. *)

let index =
  int

(* -------------------------------------------------------------------------- *)

(* Define [length] as an alias for the concrete type [int]. Equip it with a
   generator. This is used by operations such as [make] and [init]. *)

(* A large value of [max_length] leads to slow tests. E.g., changing it from
   0x100 to 0x1000 divides by 3 the number of executions per second that the
   system is able to perform, from 300/s to 100/s. *)

let max_length =
  64

let length =
  le max_length

(* -------------------------------------------------------------------------- *)

(* A fixed default element. *)

let default =
  int &&& (fun () -> -1)

(* -------------------------------------------------------------------------- *)

(* Define [top] as an alias for the concrete type [int], equipped with
   an equality function that always returns [true]. This allows us to
   not inspect an integer result. *)

let top =
  let equal = code (lazy (!^ "(fun _ _ -> true)")) (fun _ _ -> true) in
  declare_concrete_type "top" ~equal

(* -------------------------------------------------------------------------- *)

(* Define the concrete type [side]. *)


let side =
  let print side = !^ (if side = C.front then "front" else "back") in
  declare_concrete_type "side" ~print
    &&& Gen.choose [C.front; C.back]

(* -------------------------------------------------------------------------- *)

(* Define the concrete type [direction]. *)


let direction =
  let print dir = !^ (if dir = C.forward then "forward" else "backward") in
  declare_concrete_type "direction" ~print
    &&& Gen.choose [C.forward; C.backward]

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [element option]. *)

let element_option =
  let print = Print.(option int) in
  declare_concrete_type "element option" ~print

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [element list]. *)

(* This type is used both as input and output. *)

let element_list =
  declare_concrete_type "element list" ~print:Print.(list int)
  &&& Gen.(list (le max_length) next)

let element_list_of_length_at_least n =
  element_list
  &&& Gen.(list (interval_ n (n + max_length)) next)

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [(index * element) list]. *)

let index_element_list =
  let print = Print.(list (pair int int)) in
  declare_concrete_type "(index * element) list" ~print

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [(element * element) list]. *)

let element_element_list =
  index_element_list

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [element array]. *)

(* This type is used both as input and output. *)

(* When an operation returns an array, we check that the array contents are
   correct. We do not check that the array is fresh (it is not clear how we
   might do so). *)

let element_array =
  let print = Print.(array int) in
  declare_concrete_type "element array" ~print
    &&& Gen.(array (le max_length) next)

(* -------------------------------------------------------------------------- *)

(* Declare the concrete types [element segment] and [element segment option]. *)

(* These types are currently used only as the output types of operations whose
   specification is non-deterministic, such as [get_segment]. Thus, they do
   not need to be equipped with a notion of equality or with a printer. *)

let element_segment =
  declare_concrete_type "element segment"

let element_segment_option =
  declare_concrete_type "element segment option"

(* -------------------------------------------------------------------------- *)

(* Declare [element -> element -> int] as a concrete type. *)

let ordering () =
  if Gen.bool() then
    code (lazy (PPrint.string "compare")) compare
  else
    code
      (lazy (PPrint.string "(flip compare)"))
      (flip compare)

let ordering =
  declare_concrete_type "element -> element -> int"
    &&@ ordering

(* -------------------------------------------------------------------------- *)

(* Generating integers within certain ranges. *)

(* [draw length get s] draws an element from the sequence [s], which
   must be nonempty. *)

let draw length get s =
  int &&& (fun () -> get s (Gen.int (length s) ()))

let edraw =
  draw R.E.length R.E.get

let pdraw =
  draw R.P.length R.P.get

(* [itindex it] chooses an index comprised between [-1] and [length it],
   both included. *)

let itindex length it =
  interval_ (-1) (length it)

let eitindex =
  itindex R.E.Iter.length

let pitindex =
  itindex R.P.Iter.length

(* [itlength direction it] chooses a parameter [k] for an instruction of the
   form [jump direction it k]. *)

let itlength length index direction it =
  int &&& (fun () ->
    let n = length it in
    (* Choose the target of the jump. *)
    let target = Gen.int (n + 2) () - 1 in
    assert (-1 <= target && target <= n);
    (* We want [index it + sign * k = target]. *)
    let k = C.sign direction * (target - index it) in
    k
  )

let eitlength =
  itlength R.E.Iter.length R.E.Iter.index

let pitlength =
  itlength R.P.Iter.length R.P.Iter.index

(* -------------------------------------------------------------------------- *)

(* Declare certain logical properties that serve as preconditions. *)

(* Emptiness of a sequence. *)

let eempty s =
  R.E.is_empty s

let pempty s =
  R.P.is_empty s

(* Nonemptiness of a sequence. *)

let enonempty s =
  not (R.E.is_empty s)

let pnonempty s =
  not (R.P.is_empty s)

(* Validity of an ephemeral iterator. *)

let valid it =
  R.E.Iter.is_valid it

(* [finished it] means that the iterator is at a sentinel. *)

let efinished it =
  R.E.Iter.finished it

let pfinished it =
  R.P.Iter.finished it

let enotfinished it =
  not (R.E.Iter.finished it)

let pnotfinished it =
  not (R.P.Iter.finished it)

(* [final direction it] means that the iterator is at the final sentinel
   with respect to [direction]. *)

let enotfinal direction it =
  if direction = C.forward then
    R.E.Iter.index it <> R.E.Iter.length it
  else
    R.E.Iter.index it <> -1

let pnotfinal direction it =
  if direction = C.forward then
    R.P.Iter.index it <> R.P.Iter.length it
  else
    R.P.Iter.index it <> -1

(* -------------------------------------------------------------------------- *)

(* Declare Sek's abstract types. *)

(* Ephemeral sequences. *)

let esek =
  let check _model = code (lazy (!^ "E.check")) C.E.check in
  declare_abstract_type "element E.t" ~var:"e" ~check

(* Iterators on ephemeral sequences. *)

let eiter =
  let check it =
    (* The reference implementation can tell us (with certainty) whether this
       iterator is valid. This is an example where it is useful to have access
       to both the candidate iterator and its model. *)
    if R.E.Iter.is_valid it then
      (* The reference iterator is valid. Check that the candidate iterator is
         well-formed. *)
      code (lazy (!^ "E.Iter.check")) C.E.Iter.check
    else if Settings.check_iterator_validity then
      (* The reference iterator is invalid. Verify that the candidate
         implementation thinks so as well. *)
      code
        (lazy (!^ "(fun it -> assert (not (E.Iter.is_valid it)))"))
                   (fun it -> assert (not (C.E.Iter.is_valid it)))
    else
      (* The reference iterator is invalid. The candidate implementation is
         not configured to allow runtime validity checking, so we cannot do
         anything in this case. *)
      code (lazy (assert false)) (fun _it -> ())

  in
  declare_abstract_type "element E.Iter.iter" ~var:"it" ~check

(* Persistent sequences. *)

let psek =
  let check _model = code (lazy (!^ "P.check")) C.P.check in
  declare_abstract_type "element P.t" ~var:"p" ~check

(* Iterators on persistent sequences. *)

let piter =
  let check _model = code (lazy (!^ "P.Iter.check")) C.P.Iter.check in
  declare_abstract_type "element P.Iter.iter" ~var:"it" ~check

(* -------------------------------------------------------------------------- *)

(* [must_raise_empty] transforms a function of type ['a -> 'b], which may
   raise the exception [Empty], into a function of type ['a -> unit], which
   returns normally if the original function raises [Empty] and fails
   otherwise. *)

(* This custom arrow combinator indicates that a function must be transformed
   by applying [must_raise_empty] on the reference side and on the candidate
   side. (We rely on the fact that both sides use the same exception [Empty].)
   This means that this function is expected to raise [Empty]. *)

let (^!>) domain codomain =
  map
    "must_raise_empty" must_raise_empty must_raise_empty
    (domain ^^> codomain)
    (domain ^^> unit)

(* Same setup for the exception [End]. *)

let (^$>) domain codomain =
  map
    "must_raise_end" must_raise_end must_raise_end
    (domain ^^> codomain)
    (domain ^^> unit)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Declare Sek's operations. *)

(* -------------------------------------------------------------------------- *)

(* Ephemeral sequences. *)

let () =

  let spec = default ^^> esek in
  if testing ECreate then
    declare "E.create" spec R.E.create C.E.create;

  let spec = default ^^> length ^^> element ^^> esek in
  if testing EMake then
    declare "E.make" spec R.E.make C.E.make;

  let spec = default ^^> length ^^> esek in
  if testing EInit then
    declare "harness_init E.init" spec
      (harness_init R.E.init) (harness_init  C.E.init);

  (* The operation [default] has a nondeterministic specification. We check
     that it returns some value, but do not examine this value. *)
  let spec = esek ^^> top in
  if testing EDefault then
    declare "E.default" spec (fun _ -> 0) C.E.default;

  let spec = esek ^^> length in
  if testing ELength then
    declare "E.length" spec R.E.length C.E.length;

  let spec = esek ^^> bool in
  if testing EIsEmpty then
    declare "E.is_empty" spec R.E.is_empty C.E.is_empty;

  let spec = esek ^^> unit in
  if testing EClear then
    declare "E.clear" spec R.E.clear C.E.clear;

  let spec = esek ^^> esek in
  if testing ECopy then begin
    declare "E.copy ~mode:`Copy" spec
      (fun s -> R.E.copy ~mode:`Copy s)
      (fun s -> C.E.copy ~mode:`Copy s);
    declare "E.copy ~mode:`Share" spec
      (fun s -> R.E.copy ~mode:`Share s)
      (fun s -> C.E.copy ~mode:`Share s)
  end;

  let spec = esek ^^> esek ^^> unit in
  if testing EAssign then
    declare "E.assign" spec R.E.assign C.E.assign;

  let spec = side ^^> esek ^^> element ^^> unit in
  if testing EPush then
    declare "E.push" spec R.E.push C.E.push;

  (* We have two specs for [E.pop]. If the stack is empty, it must raise the
     exception [Empty]. If the stack is nonempty, it must return an element. *)
  let spec = [
    side ^^> enonempty % esek ^^> element;
    side ^^>    eempty % esek ^!> element;
  ] in
  if testing EPop then
    foreach spec (fun spec ->
      declare "E.pop" spec R.E.pop C.E.pop
    );

  let spec = side ^^> esek ^^> element_option in
  if testing EPop then
    declare "E.pop_opt" spec R.E.pop_opt C.E.pop_opt;

  let spec = [
    side ^^> enonempty % esek ^^> element;
    side ^^>    eempty % esek ^!> element;
  ] in
  if testing EPeek then
    foreach spec (fun spec ->
      declare "E.peek" spec R.E.peek C.E.peek
    );

  let spec = side ^^> esek ^^> element_option in
  if testing EPeek then
    declare "E.peek_opt" spec R.E.peek_opt C.E.peek_opt;

  let spec = esek ^&> fun s -> lt (R.E.length s) ^^> element in
  if testing EGet then
    declare "E.get" spec R.E.get C.E.get;

  let spec = esek ^&> fun s -> lt (R.E.length s) ^^> element ^^> unit in
  if testing ESet then
    declare "E.set" spec R.E.set C.E.set;

  (* The arguments of [concat] must be distinct. *)
  let spec = esek ^&> fun s1 -> ((!=) s1) % esek ^^> esek in
  if testing EConcat then
    declare "E.concat" spec R.E.concat C.E.concat;

  (* The arguments of [append] must be distinct. *)
  let spec = side ^^> esek ^&> fun s1 -> ((!=) s1) % esek ^^> unit in
  if testing EAppend then
    declare "E.append" spec R.E.append C.E.append;

  let spec = esek ^&> fun s -> le (R.E.length s) ^^> esek *** esek in
  if testing ESplit then
    declare "E.split" spec R.E.split C.E.split;

  let spec = side ^^> esek ^&> fun s -> le (R.E.length s) ^^> esek in
  if testing ECarve then
    declare "E.carve" spec R.E.carve C.E.carve;

  let spec = side ^^> esek ^&> fun s -> le (R.E.length s) ^^> unit in
  if testing ETake then
    declare "E.take" spec R.E.take C.E.take;

  let spec = side ^^> esek ^&> fun s -> le (R.E.length s) ^^> unit in
  if testing EDrop then
    declare "E.drop" spec R.E.drop C.E.drop;

  let spec = esek ^&> fun s -> le (R.E.length s) ^&> fun i -> le (R.E.length s - i) ^^> esek in
  if testing ESub then
    declare "E.sub" spec R.E.sub C.E.sub;

  let spec = direction ^^> esek ^^> element_list in
  if testing EIter then
    declare "harness_iter E.iter" spec
      (harness_iter R.E.iter) (harness_iter C.E.iter);

  let spec = direction ^^> esek ^^> index_element_list in
  if testing EIteri then
    declare "harness_iteri E.iteri" spec
      (harness_iteri R.E.iteri) (harness_iteri C.E.iteri);

  (* Because [iter_segments] is nondeterministic, it is not tested directly.
     We know that [iter] is implemented directly in terms of [iter_segments],
     so testing [iter] is good enough. *)

  (* Because [fold_left] and [fold_right] are implemented directly in terms of
     [iter], we do not bother testing them. *)

  let spec = esek ^^> element_list in
  if testing EToList then
    declare "E.to_list" spec R.E.to_list C.E.to_list;

  let spec = esek ^^> element_array in
  if testing EToArray then
    declare "E.to_array" spec R.E.to_array C.E.to_array;

  let spec = direction ^^> esek ^^> element_list in
  if testing EToSeq then
    declare "harness_to_seq E.to_seq" spec
      (harness_to_seq R.E.to_seq) (harness_to_seq C.E.to_seq);

  let spec_of_list_segment =
    default ^^> length ^&> fun n ->
    element_list_of_length_at_least n ^^>
    esek
  in
  if testing EOfListSegment then
    declare "E.of_list_segment" spec_of_list_segment
      R.E.of_list_segment C.E.of_list_segment;

  let spec_of_list = default ^^> element_list ^^> esek in
  if testing EOfList then
    declare "E.of_list" spec_of_list R.E.of_list C.E.of_list;

  let spec =
    default ^^> element_array ^&> fun a ->
    le (Array.length a) ^&> fun i ->
    le (Array.length a - i) ^^>
    esek
  in
  if testing EOfArraySegment then
    declare "E.of_array_segment" spec R.E.of_array_segment C.E.of_array_segment;

  let spec = default ^^> element_array ^^> esek in
  if testing EOfArray then
    declare "E.of_array" spec R.E.of_array C.E.of_array;

  let spec = spec_of_list_segment in
  if testing EOfSeqSegment then
    declare "harness_of_seq_segment E.of_seq_segment" spec
      (harness_of_seq_segment R.E.of_seq_segment)
      (harness_of_seq_segment C.E.of_seq_segment);

  let spec = spec_of_list in
  if testing EOfSeq then
    declare "harness_of_seq E.of_seq" spec
      (harness_of_seq R.E.of_seq)
      (harness_of_seq C.E.of_seq);

  (* Because we know that [E.find_opt] is implemented as a wrapper
     on top of [E.find], there is no need to test [E.find] directly. *)

  let spec = direction ^^> esek ^^> element_option in
  if testing EFind then
    declare "harness_find_opt E.find_opt" spec
      (harness_find_opt R.E.find_opt) (harness_find_opt C.E.find_opt);

  let spec = direction ^^> esek ^^> element_option in
  if testing EFindMap then
    declare "harness_find_map E.find_map" spec
      (harness_find_map R.E.find_map) (harness_find_map C.E.find_map);

  let spec = esek ^^> bool in
  if testing EForall then
    declare "harness_for_all E.for_all" spec
      (harness_for_all R.E.for_all) (harness_for_all C.E.for_all);

  let spec = esek ^^> bool in
  if testing EExists then
    declare "harness_exists E.exists" spec
      (harness_exists R.E.exists) (harness_exists C.E.exists);

  (* This declaration tests invocations of the form [mem x s] where by
     construction [x] does not appear in the sequence [s]. *)
  let spec = element ^^> esek ^^> bool in
  if testing EMem then
    declare "E.mem" spec R.E.mem C.E.mem;
  (* This declaration tests invocations of the form [mem x s] where [x]
     is drawn from the sequence [s]. *)
  let spec = enonempty % esek ^&> fun s -> edraw s ^^> bool in
  if testing EMem then
    declare "flip E.mem" spec (flip R.E.mem) (flip C.E.mem);

  (* Because we are using integer elements, [mem] and [memq] coincide.
     For this reason, we do not bother to test [memq]. *)

  let spec = default ^^> esek ^^> esek in
  if testing EMap then
    declare "harness_map E.map" spec
      (harness_map R.E.map) (harness_map C.E.map);

  let spec = default ^^> esek ^^> esek in
  if testing EMapi then
    declare "harness_mapi E.mapi" spec
      (harness_mapi R.E.mapi) (harness_mapi C.E.mapi);

  let spec = esek ^^> esek in
  if testing ERev then
    declare "E.rev" spec R.E.rev C.E.rev;

  (* [unzip] is implemented in one line in terms of [map], while
     [zip] is implemented in one line in terms of [map2].
     We do not bother to test them. *)

  let spec = esek ^^> esek in
  if testing EFilter then
    declare "harness_filter E.filter" spec
      (harness_filter R.E.filter) (harness_filter C.E.filter);

  let spec = default ^^> esek ^^> esek in
  if testing EFilterMap then
    declare "harness_filter_map E.filter_map" spec
      (harness_filter_map R.E.filter_map) (harness_filter_map C.E.filter_map);

  let spec = esek ^^> esek *** esek in
  if testing EPartition then
    declare "harness_partition E.partition" spec
      (harness_partition R.E.partition) (harness_partition C.E.partition);

  (* Feeling too lazy to test [E.flatten]. That would require generating a
     sequence of sequences. [E.flatten] is just three lines of code anyway. *)

  let spec = direction ^^> esek ^^> esek ^^> element_element_list in
  if testing EIter2 then
    declare "harness_iter2 E.iter2" spec
      (harness_iter2 R.E.iter2) (harness_iter2 C.E.iter2);

  (* Because [iter2_segments] is nondeterministic, it is not tested directly.
     We know that [iter2] is implemented directly in terms of
     [iter2_segments], so testing [iter2] is good enough. *)

  (* Because [fold_left2] and [fold_right2] are implemented directly in terms
     of [iter2], we do not bother testing them. *)

  let spec = default ^^> esek ^^> esek ^^> esek in
  if testing EMap2 then
    declare "harness_map2 E.map2" spec
      (harness_map2 R.E.map2) (harness_map2 C.E.map2);

  let spec = esek ^^> esek ^^> bool in
  if testing EForall2 then
    declare "harness_for_all2 E.for_all2" spec
      (harness_for_all2 R.E.for_all2) (harness_for_all2 C.E.for_all2);

  let spec = esek ^^> esek ^^> bool in
  if testing EExists2 then
    declare "harness_exists2 E.exists2" spec
      (harness_exists2 R.E.exists2) (harness_exists2 C.E.exists2);

  let spec = esek ^^> esek ^^> bool in
  if testing EEqual then
    declare "E.equal (=)" spec (R.E.equal (=)) (C.E.equal (=));

  let spec = ordering ^^> esek ^^> esek ^^> int in
  if testing ECompare then
    declare "E.compare" spec R.E.compare C.E.compare;

  let spec = ordering ^^> esek ^^> unit in
  if testing ESort then
    declare "E.sort" spec R.E.sort C.E.sort;

  let spec = ordering ^^> esek ^^> unit in
  if testing ESort then
    declare "E.stable_sort" spec R.E.stable_sort C.E.stable_sort;
  (* TODO This test does not guarantee that the sort is stable.
     If we implement our own stable sorting algorithm, we should
     sort data other than integers in order to verify that the
     algorithm is indeed stable. *)

  let spec = ordering ^^> esek ^^> esek in
  if testing EUniq then declare "E.uniq" spec R.E.uniq C.E.uniq;

  let spec = ordering ^^> esek ^^> esek ^^> esek in
  let esort cmp s = let s = R.E.copy s in R.E.sort cmp s; s in
  let csort cmp s = let s = C.E.copy s in C.E.sort cmp s; s in
  let ssort = "(fun cmp s -> let s = E.copy s in E.sort cmp s; s)" in
  if testing EMerge then
    declare
      (Printf.sprintf "harness_merge %s E.merge" ssort) spec
      (harness_merge esort R.E.merge) (harness_merge csort C.E.merge);

  let spec =
    esek ^&> fun s ->
    le (R.E.length s) ^&> fun i ->
    le (R.E.length s - i) ^^>
    element ^^>
    unit
  in
  if testing EFill then
    declare "E.fill" spec R.E.fill C.E.fill;

  let spec =
    esek ^&> fun s1 ->
    le (R.E.length s1) ^&> fun i1 ->
    esek ^&> fun s2 ->
    le (R.E.length s2) ^&> fun i2 ->
    le (min (R.E.length s1 - i1) (R.E.length s2 - i2)) ^^>
    unit
  in
  if testing EBlit then
    declare "E.blit" spec R.E.blit C.E.blit;

  ()

(* -------------------------------------------------------------------------- *)

(* Persistent sequences. *)

let () =

  let spec = default ^^> psek in
  if testing PCreate then
    declare "P.create" spec R.P.create C.P.create;

  let spec = default ^^> length ^^> element ^^> psek in
  if testing PMake then
    declare "P.make" spec R.P.make C.P.make;

  let spec = default ^^> length ^^> psek in
  if testing PInit then
    declare "harness_init P.init" spec
      (harness_init R.P.init) (harness_init  C.P.init);

  (* The operation [default] has a nondeterministic specification. We check
     that it returns some value, but do not examine this value. *)
  let spec = psek ^^> top in
  if testing PDefault then
    declare "P.default" spec (fun _ -> 0) C.P.default;

  let spec = psek ^^> length in
  if testing PLength then
    declare "P.length" spec R.P.length C.P.length;

  let spec = psek ^^> bool in
  if testing PIsEmpty then
    declare "P.is_empty" spec R.P.is_empty C.P.is_empty;

  let spec = side ^^> psek ^^> element ^^> psek in
  if testing PPush then
    declare "P.push" spec R.P.push C.P.push;

  let spec = [
    side ^^> pnonempty % psek ^^> element *** psek;
    side ^^>    pempty % psek ^!> element *** psek;
  ] in
  if testing PPop then
    foreach spec (fun spec ->
      declare "P.pop" spec R.P.pop C.P.pop
    );

  let spec = side ^^> psek ^^> element_option *** psek in
  if testing PPop then
    declare "P.pop_opt" spec R.P.pop_opt C.P.pop_opt;

  let spec = [
    side ^^> pnonempty % psek ^^> element;
    side ^^>    pempty % psek ^!> element;
  ] in
  if testing PPeek then
    foreach spec (fun spec ->
      declare "P.peek" spec R.P.peek C.P.peek
    );

  let spec = side ^^> psek ^^> element_option in
  if testing PPeek then
    declare "P.peek_opt" spec R.P.peek_opt C.P.peek_opt;

  let spec = psek ^&> fun s -> lt (R.P.length s) ^^> element in
  if testing PGet then
    declare "P.get" spec R.P.get C.P.get;

  let spec = psek ^&> fun s -> lt (R.P.length s) ^^> element ^^> psek in
  if testing PSet then
    declare "P.set" spec R.P.set C.P.set;

  let spec = psek ^^> psek ^^> psek in
  if testing PConcat then
    declare "P.concat" spec R.P.concat C.P.concat;

  let spec = psek ^&> fun s -> le (R.P.length s) ^^> psek *** psek in
  if testing PSplit then
    declare "P.split" spec R.P.split C.P.split;

  let spec = side ^^> psek ^&> fun s -> le (R.P.length s) ^^> psek in
  if testing PTake then
    declare "P.take" spec R.P.take C.P.take;

  let spec = side ^^> psek ^&> fun s -> le (R.P.length s) ^^> psek in
  if testing PDrop then
    declare "P.drop" spec R.P.drop C.P.drop;

  let spec =
    psek ^&> fun s ->
    le (R.P.length s) ^&> fun i ->
    le (R.P.length s - i) ^^>
    psek
  in
  if testing PSub then
    declare "P.sub" spec R.P.sub C.P.sub;

  let spec = direction ^^> psek ^^> element_list in
  if testing PIter then
    declare "harness_iter P.iter" spec
      (harness_iter R.P.iter) (harness_iter C.P.iter);

  let spec = direction ^^> psek ^^> index_element_list in
  if testing PIteri then
    declare "harness_iteri P.iteri" spec
      (harness_iteri R.P.iteri) (harness_iteri C.P.iteri);

  (* Because [iter_segments] is nondeterministic, it is not tested directly.
     We know that [iter] is implemented directly in terms of [iter_segments],
     so testing [iter] is good enough. *)

  (* Because [fold_left] and [fold_right] are implemented directly in terms of
     [iter], we do not bother testing them. *)

  let spec = psek ^^> element_list in
  if testing PToList then
    declare "P.to_list" spec R.P.to_list C.P.to_list;

  let spec = psek ^^> element_array in
  if testing PToArray then
    declare "P.to_array" spec R.P.to_array C.P.to_array;

  let spec = direction ^^> psek ^^> element_list in
  if testing PToSeq then
    declare "harness_to_seq P.to_seq" spec
      (harness_to_seq R.P.to_seq) (harness_to_seq C.P.to_seq);

  let spec_of_list_segment =
    default ^^> length ^&> fun n ->
    element_list_of_length_at_least n ^^>
    psek
  in
  if testing POfListSegment then
    declare "P.of_list_segment" spec_of_list_segment
      R.P.of_list_segment C.P.of_list_segment;

  let spec_of_list = default ^^> element_list ^^> psek in
  if testing POfList then
    declare "P.of_list" spec_of_list R.P.of_list C.P.of_list;

  let spec =
    default ^^> element_array ^&> fun a ->
    le (Array.length a) ^&> fun i ->
    le (Array.length a - i) ^^>
    psek
  in
  if testing POfArraySegment then
    declare "P.of_array_segment"
      spec R.P.of_array_segment C.P.of_array_segment;

  let spec = default ^^> element_array ^^> psek in
  if testing POfArray then
    declare "P.of_array" spec R.P.of_array C.P.of_array;

  let spec = spec_of_list_segment in
  if testing POfSeqSegment then
    declare "harness_of_seq_segment P.of_seq_segment" spec
      (harness_of_seq_segment R.P.of_seq_segment)
      (harness_of_seq_segment C.P.of_seq_segment);

  let spec = spec_of_list in
  if testing POfSeq then
    declare "harness_of_seq P.of_seq" spec
      (harness_of_seq R.P.of_seq)
      (harness_of_seq C.P.of_seq);

  (* Because we know that [P.find_opt] is implemented as a wrapper
     on top of [P.find], there is no need to test [P.find] directly. *)

  let spec = direction ^^> psek ^^> element_option in
  if testing PFind then
    declare "harness_find_opt P.find_opt" spec
      (harness_find_opt R.P.find_opt) (harness_find_opt C.P.find_opt);

  let spec = direction ^^> psek ^^> element_option in
  if testing PFindMap then
    declare "harness_find_map P.find_map" spec
      (harness_find_map R.P.find_map) (harness_find_map C.P.find_map);

  let spec = psek ^^> bool in
  if testing PForall then
    declare "harness_for_all P.for_all" spec
      (harness_for_all R.P.for_all) (harness_for_all C.P.for_all);

  let spec = psek ^^> bool in
  if testing PExists then
    declare "harness_exists P.exists" spec
      (harness_exists R.P.exists) (harness_exists C.P.exists);

  (* This declaration tests invocations of the form [mem x s] where by
     construction [x] does not appear in the sequence [s]. *)
  let spec = element ^^> psek ^^> bool in
  if testing PMem then
    declare "P.mem" spec R.P.mem C.P.mem;
  (* This declaration tests invocations of the form [mem x s] where [x]
     is drawn from the sequence [s]. *)
  let spec = pnonempty % psek ^&> fun s -> pdraw s ^^> bool in
  if testing PMem then
    declare "flip P.mem" spec (flip R.P.mem) (flip C.P.mem);

  (* Because we are using integer elements, [mem] and [memq] coincide.
     For this reason, we do not bother to test [memq]. *)

  let spec = default ^^> psek ^^> psek in
  if testing PMap then
    declare "harness_map P.map" spec
      (harness_map R.P.map) (harness_map C.P.map);

  let spec = default ^^> psek ^^> psek in
  if testing PMapi then
    declare "harness_mapi P.mapi" spec
      (harness_mapi R.P.mapi) (harness_mapi C.P.mapi);

  let spec = psek ^^> psek in
  if testing PRev then
    declare "P.rev" spec R.P.rev C.P.rev;

  (* [unzip] is implemented in one line in terms of [map], while
     [zip] is implemented in one line in terms of [map2].
     We do not bother to test them. *)

  let spec = psek ^^> psek in
  if testing PFilter then
    declare "harness_filter P.filter" spec
      (harness_filter R.P.filter) (harness_filter C.P.filter);

  let spec = default ^^> psek ^^> psek in
  if testing PFilterMap then
    declare "harness_filter_map P.filter_map" spec
      (harness_filter_map R.P.filter_map) (harness_filter_map C.P.filter_map);

  let spec = psek ^^> psek *** psek in
  if testing PPartition then
    declare "harness_partition P.partition" spec
      (harness_partition R.P.partition) (harness_partition C.P.partition);

  (* Feeling too lazy to test [P.flatten]. That would require generating a
     sequence of sequences. [P.flatten] is just two lines of code anyway. *)

  let spec = default ^^> psek ^^> psek in
  if testing PFlattenMap then
    declare "harness_flatten_map P.init P.flatten_map" spec
      (harness_flatten_map R.P.init R.P.flatten_map)
      (harness_flatten_map C.P.init C.P.flatten_map);

  let spec = direction ^^> psek ^^> psek ^^> element_element_list in
  if testing PIter2 then
    declare "harness_iter2 P.iter2" spec
      (harness_iter2 R.P.iter2) (harness_iter2 C.P.iter2);

  (* Because [iter2_segments] is nondeterministic, it is not tested directly.
     We know that [iter2] is implemented directly in terms of
     [iter2_segments], so testing [iter2] is good enough. *)

  (* Because [fold_left2] and [fold_right2] are implemented directly in terms
     of [iter2], we do not bother testing them. *)

  let spec = default ^^> psek ^^> psek ^^> psek in
  if testing PMap2 then
    declare "harness_map2 P.map2" spec
      (harness_map2 R.P.map2) (harness_map2 C.P.map2);

  let spec = psek ^^> psek ^^> bool in
  if testing PForall2 then
    declare "harness_for_all2 P.for_all2" spec
      (harness_for_all2 R.P.for_all2) (harness_for_all2 C.P.for_all2);

  let spec = psek ^^> psek ^^> bool in
  if testing PExists2 then
    declare "harness_exists2 P.exists2" spec
      (harness_exists2 R.P.exists2) (harness_exists2 C.P.exists2);

  let spec = psek ^^> psek ^^> bool in
  if testing PEqual then
    declare "P.equal (=)" spec (R.P.equal (=)) (C.P.equal (=));

  let spec = ordering ^^> psek ^^> psek ^^> int in
  if testing PCompare then
    declare "P.compare" spec R.P.compare C.P.compare;

  let spec = ordering ^^> psek ^^> psek in
  if testing PSort then
    declare "P.sort" spec R.P.sort C.P.sort;

  let spec = ordering ^^> psek ^^> psek in
  if testing PSort then
    declare "P.stable_sort" spec R.P.stable_sort C.P.stable_sort;

  let spec = ordering ^^> psek ^^> psek in
  if testing PUniq then
    declare "P.uniq" spec R.P.uniq C.P.uniq;

  let spec = ordering ^^> psek ^^> psek ^^> psek in
  if testing PMerge then
    declare "harness_merge P.sort P.merge" spec
      (harness_merge R.P.sort R.P.merge) (harness_merge C.P.sort C.P.merge);

  ()

(* -------------------------------------------------------------------------- *)

(* Conversions. *)

let () =

  let spec = esek ^^> psek in
  if testing ESnapshot then
    declare "snapshot" spec R.snapshot C.snapshot;

  let spec = esek ^^> psek in
  if testing ESnapshot then
    declare "snapshot_and_clear" spec
      R.snapshot_and_clear C.snapshot_and_clear;

  let spec = psek ^^> esek in
  if testing EEdit then
    declare "edit" spec R.edit C.edit;

  ()

(* -------------------------------------------------------------------------- *)

(* Iterators on ephemeral sequences. *)

let viter =
  valid % eiter

let () =

  let spec = direction ^^> esek ^^> eiter in
  if testing EICreate then
    declare "E.Iter.create" spec R.E.Iter.create C.E.Iter.create;

  (* [reset] can be applied to an invalid iterator. *)
  let spec = direction ^^> eiter ^^> unit in
  if testing IReset then
    declare "E.Iter.reset" spec R.E.Iter.reset C.E.Iter.reset;

  let spec = viter ^^> eiter in
  if testing ICopy then
    declare "E.Iter.copy" spec R.E.Iter.copy C.E.Iter.copy;

  (* The operation [sequence] should not be treated as an ordinary operation
     that produces a sequence; that would pollute our environment with a new
     sequence variable for a sequence that we already have at hand. *)

  (* What we should do, ideally, is check the physical identity of the
     sequence that is returned by this operation. That said, [sequence]
     is so unlikely to be incorrect that it does not seem worth the trouble. *)

  (* [sequence] can be applied to an invalid iterator. *)

  let spec = viter ^^> length in
  if testing ILength then
    declare "E.Iter.length" spec R.E.Iter.length C.E.Iter.length;

  let spec = viter ^^> index in
  if testing IIndex then
    declare "E.Iter.index" spec R.E.Iter.index C.E.Iter.index;

  let spec = viter ^^> bool in
  if testing IFinished then
    declare "E.Iter.finished" spec R.E.Iter.finished C.E.Iter.finished;

  let spec = [
    enotfinished % viter ^^> element;
       efinished % viter ^$> element;
  ] in
  if testing IGet then
    foreach spec (fun spec ->
      declare "E.Iter.get" spec R.E.Iter.get C.E.Iter.get
    );

  let spec = viter ^^> element_option in
  if testing IGet then
    declare "E.Iter.get_opt" spec R.E.Iter.get_opt C.E.Iter.get_opt;

  let spec = [
    direction ^^> enotfinished % viter ^^> nondet element_segment;
    direction ^^>    efinished % viter ^$> nondet element_segment
  ] in
  if testing IGetSegment then
    foreach spec (fun spec ->
      declare "E.Iter.get_segment" spec
        R.E.Iter.get_segment C.E.Iter.get_segment
    );

  let spec = direction ^^> viter ^^> nondet element_segment_option in
  if testing IGetSegment then
    declare "E.Iter.get_segment_opt" spec
      R.E.Iter.get_segment_opt C.E.Iter.get_segment_opt;

  (* Attempting to move beyond the final sentinel is forbidden. *)
  let spec =
    direction ^&> fun direction ->
    enotfinal direction % viter ^^>
    unit
  in
  if testing IMove then
    declare "E.Iter.move" spec R.E.Iter.move C.E.Iter.move;

  (* Calling [jump] requires the destination index to be within bounds. That
     is, [index it + sign direction * k] must be comprised between [-1] and
     [n], included. *)
  let spec =
    direction ^&> fun direction ->
    viter ^&> fun it ->
    eitlength direction it ^^>
    unit
  in
  if testing IJump then
    declare "E.Iter.jump" spec R.E.Iter.jump C.E.Iter.jump;

  let spec = viter ^&> fun it -> eitindex it ^^> unit in
  if testing IReach then
    declare "E.Iter.reach" spec R.E.Iter.reach C.E.Iter.reach;

  let spec = [
    direction ^^> enotfinished % viter ^^> element;
    direction ^^>    efinished % viter ^$> element;
  ] in
  if testing IGetAndMove then
    foreach spec (fun spec ->
      declare "E.Iter.get_and_move" spec
        R.E.Iter.get_and_move C.E.Iter.get_and_move
    );

  let spec = direction ^^> viter ^^> element_option in
  if testing IGetAndMove then
    declare "E.Iter.get_and_move_opt" spec
      R.E.Iter.get_and_move_opt C.E.Iter.get_and_move_opt;

  let spec = [
    direction ^^> enotfinished % viter ^^> nondet element_segment;
    direction ^^>    efinished % viter ^$> nondet element_segment;
  ] in
  if testing IGetSegmentAndJump then
    foreach spec (fun spec ->
      declare "E.Iter.get_segment_and_jump" spec
        R.E.Iter.get_segment_and_jump C.E.Iter.get_segment_and_jump
    );

  let spec = direction ^^> viter ^^> nondet element_segment_option in
  if testing IGetSegmentAndJump then
    declare "E.Iter.get_segment_and_jump_opt" spec
      R.E.Iter.get_segment_and_jump_opt C.E.Iter.get_segment_and_jump_opt;

  (* [is_valid] can be applied to an invalid iterator. *)

  (* If [check_iterator_validity] is [false], then [is_valid] is expected
     to always return true. *)

  let spec = eiter ^^> bool in
  let is_valid =
    if Settings.check_iterator_validity then
      R.E.Iter.is_valid
    else
      (fun _it -> true)
  in
  if testing IIsValid then
    declare "E.Iter.is_valid" spec
      is_valid C.E.Iter.is_valid;

  let spec = [
    enotfinished % viter ^^> element ^^> unit;
       efinished % viter ^^> element ^$> unit;
  ] in
  if testing ISet then
    foreach spec (fun spec ->
      declare "E.Iter.set" spec R.E.Iter.set C.E.Iter.set
    );

  (* TODO test [get_writable_segment], [get_writable_segment_opt] *)

  let spec = [
    direction ^^> enotfinished % viter ^^> element ^^> unit;
    direction ^^>    efinished % viter ^^> element ^$> unit;
  ] in
  if testing ISetAndMove then
    foreach spec (fun spec ->
      declare "E.Iter.set_and_move" spec
        R.E.Iter.set_and_move C.E.Iter.set_and_move
    );

  (* TODO test [get_writable_segment_and_jump],
     [get_writable_segment_opt_and_jump] *)

  ()

(* -------------------------------------------------------------------------- *)

(* Iterators on persistent sequences. *)

let () =

  let spec = direction ^^> psek ^^> piter in
  if testing PICreate then
    declare "P.Iter.create" spec R.P.Iter.create C.P.Iter.create;

  let spec = direction ^^> piter ^^> unit in
  if testing IReset then
    declare "P.Iter.reset" spec R.P.Iter.reset C.P.Iter.reset;

  let spec = piter ^^> piter in
  if testing ICopy then
    declare "P.Iter.copy" spec R.P.Iter.copy C.P.Iter.copy;

  (* The operation [sequence] should not be treated as an ordinary operation
     that produces a sequence; that would pollute our environment with a new
     sequence variable for a sequence that we already have at hand. *)

  (* What we should do, ideally, is check the physical identity of the
     sequence that is returned by this operation. That said, [sequence]
     is so unlikely to be incorrect that it does not seem worth the trouble. *)

  let spec = piter ^^> length in
  if testing ILength then
    declare "P.Iter.length" spec R.P.Iter.length C.P.Iter.length;

  let spec = piter ^^> index in
  if testing IIndex then
    declare "P.Iter.index" spec R.P.Iter.index C.P.Iter.index;

  let spec = piter ^^> bool in
  if testing IFinished then
    declare "P.Iter.finished" spec R.P.Iter.finished C.P.Iter.finished;

  let spec = [
    pnotfinished % piter ^^> element;
       pfinished % piter ^$> element;
  ] in
  if testing IGet then
    foreach spec (fun spec ->
      declare "P.Iter.get" spec R.P.Iter.get C.P.Iter.get
    );

  let spec = piter ^^> element_option in
  if testing IGet then
    declare "P.Iter.get_opt" spec R.P.Iter.get_opt C.P.Iter.get_opt;

  let spec = [
    direction ^^> pnotfinished % piter ^^> nondet element_segment;
    direction ^^>    pfinished % piter ^$> nondet element_segment;
  ] in
  if testing IGetSegment then
    foreach spec (fun spec ->
      declare "P.Iter.get_segment" spec
        R.P.Iter.get_segment C.P.Iter.get_segment
    );

  let spec = direction ^^> piter ^^> nondet element_segment_option in
  if testing IGetSegment then
    declare "P.Iter.get_segment_opt" spec
      R.P.Iter.get_segment_opt C.P.Iter.get_segment_opt;

  (* Attempting to move beyond the final sentinel is forbidden. *)
  let spec =
    direction ^&> fun direction ->
    pnotfinal direction % piter ^^>
    unit
  in
  if testing IMove then
    declare "P.Iter.move" spec R.P.Iter.move C.P.Iter.move;

  (* Calling [jump] requires the destination index to be within bounds. That
     is, [index it + sign direction * k] must be comprised between [-1] and
     [n], included. *)
  let spec =
    direction ^&> fun direction ->
    piter ^&> fun it ->
    pitlength direction it ^^>
    unit
  in
  if testing IJump then
    declare "P.Iter.jump" spec R.P.Iter.jump C.P.Iter.jump;

  let spec = piter ^&> fun it -> pitindex it ^^> unit in
  if testing IReach then
    declare "P.Iter.reach" spec R.P.Iter.reach C.P.Iter.reach;

  let spec = [
    direction ^^> pnotfinished % piter ^^> element;
    direction ^^>    pfinished % piter ^$> element;
  ] in
  if testing IGetAndMove then
    foreach spec (fun spec ->
      declare "P.Iter.get_and_move" spec
        R.P.Iter.get_and_move C.P.Iter.get_and_move
    );

  let spec = direction ^^> piter ^^> element_option in
  if testing IGetAndMove then
    declare "P.Iter.get_and_move_opt" spec
      R.P.Iter.get_and_move_opt C.P.Iter.get_and_move_opt;

  let spec = [
    direction ^^> pnotfinished % piter ^^> nondet element_segment;
    direction ^^>    pfinished % piter ^$> nondet element_segment;
  ] in
  if testing IGetSegmentAndJump then
    foreach spec (fun spec ->
      declare "P.Iter.get_segment_and_jump" spec
        R.P.Iter.get_segment_and_jump C.P.Iter.get_segment_and_jump
    );

  let spec = direction ^^> piter ^^> nondet element_segment_option in
  if testing IGetSegmentAndJump then
    declare "P.Iter.get_segment_and_jump_opt" spec
      R.P.Iter.get_segment_and_jump_opt C.P.Iter.get_segment_and_jump_opt;
  ()

(* -------------------------------------------------------------------------- *)

end (* Prologue *)

(* Run! *)

let () =
  let prologue () = let module P = Prologue() in () in
  let fuel = 10 in
  main ~prologue fuel
