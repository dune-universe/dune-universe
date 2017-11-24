module type Kernel1 = sig
  type 'a t

  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module type Kernel2 = sig
  type ('a, 'b) t

  val return: 'a -> ('a, _) t
  val bind: ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end

module type S1 = sig
  type 'a t

  module Core: sig
    include Kernel1 with type 'a t := 'a t
  end
  include module type of Core

  module Infix: sig
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type S2 = sig
  type ('a, 'b) t

  module Core: sig
    include Kernel2 with type ('a, 'b) t := ('a, 'b) t

    val return: 'a -> ('a, _) t
  end
  include module type of Core

  module Infix: sig
    val (>>=): ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  end
end

module Make1(K: Kernel1) = struct
  type 'a t = 'a K.t

  module Core = struct
    let return = K.return
    let bind = K.bind
  end
  include Core

  module Infix = struct
    let (>>=) = bind
  end
end

module Make2(K: Kernel2) = struct
  type ('a, 'b) t = ('a, 'b) K.t

  module Core = struct
    let return = K.return
    let bind = K.bind
  end
  include Core

  module Infix = struct
    let (>>=) = bind
  end
end
