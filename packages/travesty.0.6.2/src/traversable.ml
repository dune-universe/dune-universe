(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019, 2020 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base
open Traversable_types

(** [Derived_ops_maker] is an internal module type used for implementing the
    derived operations (fold-map, fold, iterate) in an arity-generic way. *)
module type Derived_ops_maker = sig
  include Generic_types.Generic

  module On_monad (M : Monad.S) :
    Basic_generic_on_monad
      with module M := M
       and type 'a t := 'a t
       and type 'a elt := 'a elt
end

(** [Derived_ops_monadic_gen] is an internal functor used to generate several
    derived monadic operations (monadic fold-map, monadic iteration, etc)
    from a monadic traversal in an arity-generic way. *)
module Derived_ops_monadic_gen (I : Derived_ops_maker) (M : Monad.S) = struct
  (* We use the state monad to implement fold-map. *)
  module SM = State_transform.Make2 (M)
  module IM = I.On_monad (M)

  let fold_map_m (type acc) c ~f ~init =
    let module SM' =
      State_transform.To_S
        (SM)
        (struct
          type t = acc
        end)
    in
    let module ISM = I.On_monad (SM') in
    SM.run' (ISM.map_m ~f:(fun x -> SM.Monadic.make (fun s -> f s x)) c) init

  let fold_m c ~init ~f =
    M.(
      fold_map_m ~init c ~f:(fun k x -> f k x >>| fun x' -> (x', x)) >>| fst)

  let iter_m c ~f =
    M.(IM.map_m ~f:(fun x -> M.(f x >>| fun () -> x)) c >>| fun _ -> ())

  let mapi_m ~f c =
    M.(
      fold_map_m ~init:0 c ~f:(fun k x -> f k x >>| fun x' -> (k + 1, x'))
      >>| snd)
end

(** Internal functor for generating several derived non-monadic,
    non-[Container] operations (map, iterate) from a fold-map, generic over
    both arity-0 and arity-1. *)
module Derived_ops_gen (I : Derived_ops_maker) = struct
  (* As usual, we just use the monadic equivalents over the identity monad. *)
  module D = Derived_ops_monadic_gen (I) (Monad.Ident)

  let fold_map = D.fold_map_m

  let mapi = D.mapi_m
end

(* Basic-signature modules need a bit of rearrangement to fit in the derived
   operation functors. *)

(** Internal functor for rearranging arity-0 basics to derived-ops makers. *)
module Basic0_to_derived_ops_maker (I : Basic0) :
  Derived_ops_maker
    with type 'a t = I.t
     and type 'a elt = I.Elt.t
     and module On_monad = I.On_monad = struct
  type 'a t = I.t

  type 'a elt = I.Elt.t

  module On_monad = I.On_monad
end

(** Internal functor for rearranging arity-1 basics to derived-ops makers. *)
module Basic1_to_derived_ops_maker (I : Basic1) :
  Derived_ops_maker
    with type 'a t = 'a I.t
     and type 'a elt = 'a
     and module On_monad = I.On_monad = struct
  type 'a t = 'a I.t

  type 'a elt = 'a

  module On_monad = I.On_monad
end

(** [Container_gen] is an internal functor used to generate the input to
    [Container] functors in an arity-generic way. *)
module Container_gen (I : Derived_ops_maker) : sig
  val fold : 'a I.t -> init:'acc -> f:('acc -> 'a I.elt -> 'acc) -> 'acc

  val iter : [> `Custom of 'a I.t -> f:('a I.elt -> unit) -> unit]

  val length : [> `Define_using_fold]
end = struct
  module D = Derived_ops_monadic_gen (I) (Monad.Ident)

  let fold = D.fold_m

  let iter = `Custom D.iter_m

  let length = `Define_using_fold
end

module Make0_container (I : Basic0_container) :
  S0 with module Elt = I.Elt and type t = I.t = struct
  module Maker = Basic0_to_derived_ops_maker (I)
  module Elt = I.Elt

  type elt = I.Elt.t

  (* We can implement the non-monadic map using the identity monad. *)
  module Ident = I.On_monad (Monad.Ident)

  let map = Ident.map_m

  include Derived_ops_gen (Maker)

  include (I : Container.S0 with type t = I.t and type elt := elt)

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)
    include Derived_ops_monadic_gen (Maker) (MS)
  end

  module With_errors = On_monad (Or_error)
end

module Make0 (I : Basic0) : S0 with module Elt = I.Elt and type t = I.t =
Make0_container (struct
  include I

  include Container.Make0 (struct
    include I
    include Container_gen (Basic0_to_derived_ops_maker (I))
  end)
end)

module Make1_container (I : Basic1_container) : S1 with type 'a t = 'a I.t =
struct
  type nonrec 'a t = 'a I.t

  (* [I] needs a bit of rearrangement to fit in the derived operation
     functors (as above, but slightly differently). *)
  module Maker = Basic1_to_derived_ops_maker (I)
  module Ident = I.On_monad (Monad.Ident)

  let map = Ident.map_m

  include Derived_ops_gen (Maker)

  module C : Container.S1 with type 'a t := 'a I.t = I

  include C

  include Mappable.Extend1 (struct
    type nonrec 'a t = 'a I.t

    let map = map

    include C
  end)

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)
    include Derived_ops_monadic_gen (Maker) (MS)

    (* [sequence_m] can't be defined on arity-0 containers. *)
    let sequence_m c = map_m ~f:Fn.id c
  end

  module With_errors = On_monad (Base.Or_error)
end

module Make1 (I : Basic1) : S1 with type 'a t = 'a I.t =
Make1_container (struct
  include I

  include Container.Make (struct
    type nonrec 'a t = 'a I.t

    include Container_gen (Basic1_to_derived_ops_maker (I))
  end)
end)

module Chain0 (Outer : S0) (Inner : S0 with type t := Outer.Elt.t) :
  S0 with module Elt = Inner.Elt and type t = Outer.t = Make0 (struct
  type t = Outer.t

  module Elt = Inner.Elt

  module On_monad (M : Monad.S) = struct
    module OM = Outer.On_monad (M)
    module IM = Inner.On_monad (M)

    let map_m x ~f = OM.map_m x ~f:(IM.map_m ~f)
  end
end)

module Fix_elt (I : S1) (Elt : Equal.S) :
  S0 with type t = Elt.t I.t and module Elt = Elt = Make0 (struct
  type t = Elt.t I.t

  module Elt = Elt

  (* The [S0] fold-map has a strictly narrower function type than the [S1]
     one, so we can just supply the same [On_monad]. *)
  module On_monad (M : Monad.S) = I.On_monad (M)
end)

module Const (T : T) (Elt : Equal.S) = Make0 (struct
  type t = T.t

  module Elt = Elt

  module On_monad (M : Monad.S) = struct
    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      ignore f ; M.return x
  end
end)

module Helpers (M : Monad.S) = struct
  type 'a traversal = 'a -> 'a M.t

  let proc_variant0 f v = M.(f () >>| fun () -> v.Base.Variant.constructor)

  let proc_variant1 f v a = M.(f a >>| v.Base.Variant.constructor)

  let proc_variant2 f v a b =
    let open M.Let_syntax in
    let%map a', b' = f (a, b) in
    v.Base.Variant.constructor a' b'

  let proc_variant3 f v a b c =
    let open M.Let_syntax in
    let%map a', b', c' = f (a, b, c) in
    v.Base.Variant.constructor a' b' c'

  let proc_field f state field =
    let open M.Let_syntax in
    let%bind container = state in
    let original = Field.get field container in
    let%map nval = f original in
    Field.fset field container nval
end
