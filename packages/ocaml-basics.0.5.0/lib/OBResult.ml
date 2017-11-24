module type S = sig
  module Accu: OBMonoid.S
  module Monad: OBMonad.S2
    with type ('ok, 'err) t = ('ok, 'err) result
  module Applicative: OBApplicative.S2
    with type ('ok, 'err) t = ('ok, 'err) result

  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err
    [@@deriving sexp]

  include module type of Monad.Core
  include module type of Applicative.Core
  include OBTraversable.S2
    with type ('a, 'b) t := ('a, 'b) t
  include OBFoldable.S2
    with type ('a, 'b) t := ('a, 'b) t

  val choose:
    ('ok, 'err Accu.t) t
    -> ('ok, 'err Accu.t) t
    -> ('ok, 'err Accu.t) t

  val get_ok: default: 'a -> ('a, _) t -> 'a

  module Infix: sig
    include module type of Monad.Infix
    include module type of Applicative.Infix

    val (<|>):
      ('ok, 'err Accu.t) t
      -> ('ok, 'err Accu.t) t
      -> ('ok, 'err Accu.t) t
  end
end

module Make(Accu: OBMonoid.S): S
  with module Accu = Accu
= struct
  module Accu = Accu

  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err
    [@@deriving sexp]

  module Kernel = struct
    type nonrec ('ok, 'err) t = ('ok, 'err) t
    let return x = Ok x

    let bind m f =
      match m with
      | Ok x -> f x
      | Error x -> Error x
  end

  module Monad = OBMonad.Make2(Kernel)
  include Monad.Core

  module Applicative = OBApplicative.Make2(Kernel)
  include Applicative.Core

  include OBTraversable.Make2(Monad)
  include OBFoldable.Make2(Monad)

  let choose lhs rhs =
    match lhs, rhs with
    | Ok _, _ -> lhs
    | _, Ok _ -> rhs
    | Error a, Error b -> Error (Accu.add a b)

  let get_ok ~default = function
    | Ok x -> x
    | Error _ -> default

  module Infix = struct
    include Monad.Infix
    include Applicative.Infix
    let (<|>) = choose
  end
end

module ListMonoid = OBMonoid.Make(struct
  type 'a t = 'a list

  let add = List.append
  let zero = []
end)

include Make(ListMonoid)
