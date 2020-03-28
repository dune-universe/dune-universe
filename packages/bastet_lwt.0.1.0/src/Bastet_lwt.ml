module Functor : Bastet.Interface.FUNCTOR with type 'a t = 'a Lwt.t = struct
  type 'a t = 'a Lwt.t

  let map = Lwt.map
end

module Apply : Bastet.Interface.APPLY with type 'a t = 'a Lwt.t = struct
  include Functor

  let apply f x = Lwt.bind f (fun f' -> Lwt.map f' x)
end

module Applicative : Bastet.Interface.APPLICATIVE with type 'a t = 'a Lwt.t = struct
  include Apply

  let pure = Lwt.return
end

module Monad : Bastet.Interface.MONAD with type 'a t = 'a Lwt.t = struct
  include Applicative

  let flat_map = Lwt.bind
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
