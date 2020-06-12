open! Base
open! Import

include sig
  type 'a t =
    | None
    | Some of 'a
  [@@deriving accessors]
end
with type 'a t := 'a Option.t

(** [default x ~is_default] is an isomorphism between an [option] and its contents, where
    [None] is considered the same as [default]. [is_default x] is expected to be [true].

    - [get (default _ ~is_default:_) (Some y) = y]
    - [get (default x ~is_default:_) None = x]
    - [construct (default _ ~is_default) y = if is_default y then None else Some y]

    Note that, as explained in the [Accessor] documentation, well-behaved isomorphisms are
    expected to satisfy the following properties:

    - [get (construct b) = b]
    - [construct (get at) = at]

    However, if [is_default b = true], both properties can be violated:

    - [get (construct b) = get None = x]  (violation if [x <> b])
    - [construct (get (Some b)) = construct b = None] *)
val default
  :  'a
  -> is_default:('b -> bool)
  -> ('i -> 'a -> 'b, 'i -> 'a option -> 'b option, [< isomorphism ]) Accessor.t

include Accessor.Monad.S with type 'a t := 'a option
