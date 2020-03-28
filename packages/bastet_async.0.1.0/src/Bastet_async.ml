module Functor : Bastet.Interface.FUNCTOR with type 'a t = 'a Async_kernel.Deferred.t = struct
  type 'a t = 'a Async_kernel.Deferred.t

  let map f x = Async_kernel.Deferred.map ~f x
end

module Apply : Bastet.Interface.APPLY with type 'a t = 'a Async_kernel.Deferred.t = struct
  include Functor

  let apply f x = Async_kernel.Deferred.bind f ~f:(fun f' -> map f' x)
end

module Applicative : Bastet.Interface.APPLICATIVE with type 'a t = 'a Async_kernel.Deferred.t =
struct
  include Apply

  let pure = Async_kernel.Deferred.return
end

module Monad : Bastet.Interface.MONAD with type 'a t = 'a Async_kernel.Deferred.t = struct
  include Applicative

  let flat_map x f = Async_kernel.Deferred.bind ~f x
end

module Infix = struct
  include Bastet.Infix.Functor (Functor)
  include Bastet.Infix.Monad (Monad)
end

module List = struct
  module Traversable = Bastet.List.Traversable (Applicative)
end

module Array = struct
  module Traversable = Bastet.Array.Traversable (Applicative)
end
