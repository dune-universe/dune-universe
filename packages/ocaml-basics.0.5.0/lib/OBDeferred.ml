module type S = sig
  module Backend: OBMonad.S1

  type ('ok, 'err) t = ('ok, 'err) OBResult.t Backend.t

  module Monad: OBMonad.S2
    with type ('ok, 'err) t := ('ok, 'err) t
  module Applicative: OBApplicative.S2
    with type ('ok, 'err) t := ('ok, 'err) t

  include module type of Monad.Core
  include module type of Applicative.Core
  include OBTraversable.S2
    with type ('a, 'b) t := ('a, 'b) t
  include OBFoldable.S2
    with type ('a, 'b) t := ('a, 'b) t

  module Infix: sig
    include module type of Monad.Infix
    include module type of Applicative.Infix
  end

  val choose: ('ok, 'err list) t list -> 'err -> ('ok, 'err list) t
  val choose': ('ok, string list) t list -> ('ok, string list) t
  val error: 'err -> (_, 'err) t
end

module Make (Backend: OBMonad.S1): S
  with module Backend = Backend
= struct
  module Backend = Backend

  type ('ok, 'err) t = ('ok, 'err) OBResult.t Backend.t

  module Kernel = struct
    type nonrec ('ok, 'err) t = ('ok, 'err) t

    let bind thread f =
      Backend.bind thread (fun result ->
          match result with
          | Ok x -> f x
          | Error x -> Backend.return (Error x))

    let return x = x |> OBResult.return |> Backend.return
  end

  module Monad = OBMonad.Make2(Kernel)
  module Applicative = OBApplicative.Make2(Kernel)

  include Monad.Core
  include Applicative.Core
  include OBTraversable.Make2(Monad)
  include OBFoldable.Make2(Monad)

  module Infix = struct
    include Monad.Infix
    include Applicative.Infix
  end

  let error l = Backend.return (Error l)

  let choose deferred_list fallback =
    let rec choose accu deferred_list =
      match deferred_list with
      | [] -> error accu
      | head :: tail ->
        Backend.bind head (fun result ->
            match result with
            | Ok _ -> head
            | Error x -> choose (accu @ x) tail) in
    choose [fallback] deferred_list

  let choose' deferred_list = choose deferred_list "OBDeferred.choose"
end
