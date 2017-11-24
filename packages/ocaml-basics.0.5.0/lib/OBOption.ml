module Kernel = struct
  type 'a t = 'a option

  let bind m f =
    match m with
    | Some x -> f x
    | None -> None

  let return x = Some x
end

module Monad = OBMonad.Make1(Kernel)
module Applicative = OBApplicative.Make1(Kernel)

include Monad.Core
include Applicative.Core
include OBTraversable.Make1(Monad)
include OBFoldable.Make1(Monad)

module Infix = struct
  include Monad.Infix
  include Applicative.Infix
end
