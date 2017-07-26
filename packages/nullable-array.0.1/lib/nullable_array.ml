(************************************************************************)
(*  nullable-array                                                      *)
(*                                                                      *)
(*    Copyright 2016 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the MIT License         *)
(*                                                                      *)
(* Permission is hereby granted, free of charge, to any person          *)
(* obtaining a copy of this software and associated documentation files *)
(* (the "Software"), to deal in the Software without restriction,       *)
(* including without limitation the rights to use, copy, modify, merge, *)
(* publish, distribute, sublicense, and/or sell copies of the Software, *)
(* and to permit persons to whom the Software is furnished to do so,    *)
(* subject to the following conditions:                                 *)
(*                                                                      *)
(* The above copyright notice and this permission notice shall be       *)
(* included in all copies or substantial portions of the Software.      *)
(*                                                                      *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,      *)
(* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF   *)
(* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                *)
(* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS  *)
(* BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN   *)
(* ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN    *)
(* CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(* SOFTWARE.                                                            *)
(*                                                                      *)
(************************************************************************)

(*
   Designed and tested for OCaml <= 4.04 bytecode and native,
   Taking into consideration:
   * Dynamic float arrays
   * Marshalling
   * no-naked-pointer variant
   * structural equality/comparison functions

   If future version of OCaml runtime and compiler break some
   expected invariants, this library will be updated accordingly.

   Please do not use it with any non-blessed version. Consider
   using the slower, but safer Option array variant.

   Also in general: Please do not write code like that. This rely on
   many compiler implementation details that are impossible to get
   right without knowing the compiler internals !
   --
   Pierre Chambart
*)

(* A size n nullable array is a size n+1 array containing a reference
   null value at index 0.

   The n-th field of [a] is considered null iff [a.(n+1) == a.(0)]

   Setting the n-th field to null is done by [a.(n+1) <- a.(0)]

   Setting the n-th field to a given value [v] is simply [a.(n+1) <- v]

   This reference null value in field 0 is required since
   marshalling/unmarshalling destroy sharing (breaking physical
   equality). (See null definition for more details)

   The reference null is in field 0 to avoid needing to read the size
   for accessing it. It implies that any other access requires some
   arithmetic, but this is already the case in OCaml as integers are
   tagged.


   Since nullable arrays can contain both pointers (the null value is
   a pointer) and floating point values, great care is taken to ensure
   that the compiler consider a nullable array as an array containing
   either pointer or integers. This means that float values will be
   boxed and that not dynamic tag check occur when building or
   accessing a nullable array. To achieve this, every manipulation
   must be explicitely annotated with the ['a elt] type.

*)

[@@@ocaml.warning "-37"] (* The type elt is never built *)

type elt =
  | Constant
  | Allocated of int
  (* No value of this type will ever be created, but this type
     annotation is important to ensure that the compiler consider
     those values as 'anything that is not a float'

     When this property is required, the code should be completely
     explicitely annotated. *)

type null = { mutable null : int }
let null : elt = Obj.magic { null = 0 }
(* The null value is encoded as a value for which the semantics of
   physical equality is completely defined.

   We could either choose a mutable value or an exception. Exceptions
   have the nice benefic of being able to be statically allocated as
   they are not mutable, but this is problematic for structural
   equality: exceptions are structuraly equal if they are physicaly
   equal, which we cannot ensure when a value is built by
   unmarshalling. There is only a single instance of the null value
   explicitely created, but other instances can be created through
   unmarshal.

   Note that the 0 value could have been use with the classic
   runtime, but would break the no-naked-pointer invariant of never
   having a pointer outside of the heap that does not look like a
   black OCaml value, which of course cannot be achieved with 0.
   Future version of the runtime could be made to understant the
   0 pointer. If this happen, this value could be created as:

   {[
     external int_as_pointer : int -> elt = "%int_as_pointer"
     let null : elt = int_as_pointer 0

     let nullf () : elt = int_as_pointer 0 [@@ocaml.inline]
   ]}

   Note that nullf might be faster in general since the compiler might
   not able to constant fold the primitive (which will probably never
   happen as this is not a valid OCaml value and has no valid
   representation in either closure or flambda approximations) it will
   still be able to inline the function which will easily produce the
   expected efficient code. *)

type 'a t = elt array

let make (n:int) : 'a t =
  (* This is annotated with the type to ensure that transcore
     specialize it to the elt type. i.e. it is represented as an
     array that can contain integer and pointers, but cannot contain
     floating point values. *)

  (* The array contains n+1 elements: the first one is used to store
     the reference null. *)
  if n < 0 then invalid_arg "Nullable_array.make";
  (* There is no need to check for the upper bound as [Array.make]
     already does it *)
  Array.make (n+1) (null:elt)

let empty_array : 'a t = [| null |]

let get_null (a:'a t) : elt =
  (* An array of type ['a t] cannot be empty *)
  Array.unsafe_get (a:'a t) 0
[@@ocaml.inline]

let get (a:'a t) (n:int) : 'a option =
  (* There is no need to check for the upper bound as [Array.get]
     already does it *)
  if n < 0 then invalid_arg "Nullable_array.get";
  let elt = Array.get (a:'a t) (n+1) in
  let null = get_null a in
  if elt == null then
    None
  else
    Some (Obj.magic elt:'a)

let length (a:'a t) = Array.length a - 1

let set_elt (a:'a t) (n:int) (v:elt) =
  (* If the analysis finds out that v is a floating point value, this
     prevents the conversion of the set to a float array set *)
  Array.set a n (Sys.opaque_identity v)

let unsafe_set_elt (a:'a t) (n:int) (v:elt) =
  Array.unsafe_set a n (Sys.opaque_identity v)

let set_some (a:'a t) (n:int) (v:'a) : unit =
  if n < 0 then invalid_arg "Nullable_array.set_some";
  set_elt (a:'a t) (n+1) (Obj.magic v : elt)

let clear (a:'a t) (n:int) : unit =
  if n < 0 then invalid_arg "Nullable_array.clear";
  let null = get_null a in
  set_elt (a:'a t) (n+1) null

let set (a:'a t) (n:int) (v:'a option) : unit =
  if n < 0 then invalid_arg "Nullable_array.set_some";
  match v with
  | None ->
    let null = get_null a in
    set_elt (a:'a t) (n+1) null
  | Some v ->
    set_elt (a:'a t) (n+1) (Obj.magic v : elt)

let iteri ~(some:int -> 'a -> unit) ~(none:int -> unit) (a:'a t) : unit =
  let null = get_null a in
  for i = 1 to Array.length a - 1 do
    let elt = Array.unsafe_get a i in
    if elt != null then
      some (i-1) (Obj.magic elt:'a)
    else
      none (i-1)
  done
[@@ocaml.inline]

let unsafe_manual_blit (from:'a t) (from_start:int) (to_:'a t) (to_start:int) (len:int) =
  let null_from = get_null from in
  let null_to = get_null to_ in
  for i = 0 to len - 1 do
    let v = from.(i + from_start + 1) in
    if v == null_from then
      to_.(i + to_start + 1) <- null_to
    else
      to_.(i + to_start + 1) <- v
  done

let blit (from:'a t) (from_start:int) (to_:'a t) (to_start:int) (len:int) =
  if len < 0 || from_start < 0 || from_start > length from - len
     || to_start < 0 || to_start > length to_ - len
  then invalid_arg "Nullable_array.blit"
  else begin
    (* If both null are the same, we can optimize by using the version
       from the runtime. Since the only way for those not to be equal is
       related to some marshaling/unmarshaling, this hold in the general
       case, hence the [@inlined never] attibute on the other branch. *)
    let null_from = get_null from in
    let null_to = get_null to_ in
    if null_from == null_to then
      Array.blit from (from_start + 1) to_ (to_start + 1) len
    else
      (unsafe_manual_blit [@inlined never]) from from_start to_ to_start len
  end

let equal (a1:'a t) (a2:'a t) ~(equal:'a -> 'a -> bool) =
  length a1 = length a2 &&
  let null1 = get_null a1 in
  let null2 = get_null a2 in
  let rec loop i =
    (* i is a nullable_array index, i.e. it is in the
       interval [1, Array.length a1 - 1] *)
    if i < 1 then true
    else
      let v1 = Array.unsafe_get (a1:'a t) i in
      let v2 = Array.unsafe_get (a2:'a t) i in
      match v1 == null1, v2 == null2 with
      | true, true ->
        loop (i-1)
      | false, false ->
        let v1' : 'a = Obj.magic (v1:elt) in
        let v2' : 'a = Obj.magic (v2:elt) in
        equal v1' v2' && loop (i-1)
      | false, true | true, false ->
        false
  in
  loop (length a1)

(* Unsafe functions *)

let unsafe_get_some (a:'a t) (n:int) : 'a =
  let elt = Array.unsafe_get (a:'a t) (n+1) in
  (Obj.magic elt:'a)

let unsafe_set_some (a:'a t) (n:int) (v:'a) : unit =
  unsafe_set_elt (a:'a t) (n+1) (Obj.magic v : elt)

let unsafe_clear (a:'a t) (n:int) : unit =
  let null = get_null a in
  unsafe_set_elt (a:'a t) (n+1) null
