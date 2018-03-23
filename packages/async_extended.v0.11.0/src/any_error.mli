open! Core
open! Async

(** A variant of [Deferred.Result.t] with "any error" semantics. When multiple deferred
    values that each may be an error are combined, the result may fail with any of the
    component errors, instead of deterministically choosing a particular one. This enables
    the result to become determined sooner: for example, [Deferred.Or_error.all] gives the
    "leftmost" error if any, so if only the last element of the list is an error, the
    result will still not be determined until every other element has become so. By
    contrast, [Any_error.all ts] may become determined with an error as soon as any
    element of [ts] becomes determined with an error.

    Note that there is no monad interface, since in a monadic bind, whether or not later
    deferreds become errors may depend on the value of earlier ones, so "any-error"
    doesn't make sense in that context. However, the equivalence with
    [Deferred.Result.t] is exposed, so you can just use that monad interface where
    necessary. *)
type ('ok, 'err) t = ('ok, 'err) Deferred.Result.t

include Applicative.S2 with type ('ok, 'err) t := ('ok, 'err) t

val all_unit : (unit, 'err) t list -> (unit, 'err) t
