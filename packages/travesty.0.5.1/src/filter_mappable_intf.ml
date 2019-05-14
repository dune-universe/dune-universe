(* This file is part of 'travesty'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Signatures for filter-mapping.

    The {{!basics} basic signatures}, which contain only
    {{!filter_map} filter_map}, are {{!Basic0} Basic0} and
    {{!Basic1} Basic1}. We also define {{!sigs} richer signatures},
    {{!S0} S0} and {{!S1} S1}, for which we provide functors in
    {{!Filter_mappable} the main interface}. *)

open Base

(** {2:basics Basic signatures}

    The basic signatures are {{!Basic0} Basic0}, which defines filtered
    mapping across an arity-0 type [t] (with a fixed, associated element
    type [elt]), and {{!Basic1} Basic1}, which defines filtered mapping
    across an arity-1 type ['a t] (with element type ['a]). *)

(** {3 The generic basic signature}

    As with {{!Traversable} Traversable} and {{!Mappable} Mappable}, we
    define the basic and full signatures of filter-mappable structures in an
    arity-generic way, then specialise them for arity-0 and arity-1 types. *)

(** Generic input shape of filter-mappable structures on either an arity-0
    or arity-1 type.

    - For arity-0 types, use {{!Basic0} Basic0}: ['a t] becomes [t], and
      ['a elt] becomes [elt];
    - For arity-1 types, use {{!Basic1} Basic1}: ['a t] becomes ['a t], and
      ['a elt] becomes ['a]. *)
module type Generic_basic = sig
  (** [Generic_basic] refers to the container type as ['a t], and the
      element type as ['a elt]; substitute [t]/[elt] (arity-0) or
      ['a t]/['a] (arity-1) accordingly below. *)
  include Types_intf.Generic

  val filter_map : 'a t -> f:('a elt -> 'b elt option) -> 'b t
  (** [filter_map c ~f] maps [f] over every [t] in [c], discarding any items
      for which [f] returns [None]. *)
end

(** {3 Arity-0 and arity-1 basic signatures} *)

(** Basic signature of an arity-0 filter-mappable type.

    Functions mapped over arity-0 types must preserve the element type. *)
module type Basic0 = sig
  include Types_intf.S0

  include Generic_basic with type 'a t := t and type 'a elt := elt
end

(** Basic signature of an arity-1 mappable type.

    Functions mapped over arity-1 types may change the element type. *)
module type Basic1 = sig
  (** The type of the container to map over. *)
  type 'a t

  include Generic_basic with type 'a t := 'a t and type 'a elt := 'a
end

(** {2:sigs Full signatures}

    The full signatures are {{!S0} S0} (arity-0) and {{!S1} S1} (arity-1). *)

(** {3 The generic signature} *)

(** Generic output shape of filter-mappable structures on either an arity-0
    or arity-1 type.

    - For arity-0 types, use {{!S0} S0}: ['a t] becomes [t], and ['a elt]
      becomes [elt]; - For arity-1 types, use {{!S1} S1}: ['a t] becomes
      ['a t], and ['a elt] becomes ['a]. *)
module type Generic = sig
  (** [Generic] strictly extends [Generic_basic]. *)
  include Generic_basic

  val filter : 'a t -> f:('a elt -> bool) -> 'a t
  (** [filter c ~f] checks [f] over every [t] in [c], discarding any items
      for which [f] returns [false]. *)

  val exclude : 'a t -> f:('a elt -> bool) -> 'a t
  (** [exclude c ~f] checks [f] over every [t] in [c], discarding any items
      for which [f] returns [true]. *)

  (** We can also derive {{!Mappable} Mappable} interfaces from
      filter-mappable ones, but leave that to a separate functor. *)
end

(** {3 Arity-0 and arity-1 full signatures} *)

(** Full signature of an arity-0 filter-mappable type.

    Functions mapped over arity-0 types must preserve the element type. *)
module type S0 = sig
  include Types_intf.S0

  include Generic with type 'a t := t and type 'a elt := elt
end

(** Full signature of an arity-1 mappable type.

    Functions mapped over arity-1 types may change the element type. *)
module type S1 = sig
  (** The type of the container to map over. *)
  type 'a t

  include Generic with type 'a t := 'a t and type 'a elt := 'a
end
