module type S1 = sig
  type 'a t

  module Core: sig
    val apply: ('a -> 'b) t -> 'a t -> 'b t
    val map: ('a -> 'b) -> 'a t -> 'b t
    val pure: 'a -> 'a t
  end
  include module type of Core

  module Infix: sig
    val (<$>): ('a -> 'b) -> 'a t -> 'b t
    val (>>|): 'a t -> ('a -> 'b) -> 'b t
    val (<*>): ('a -> 'b) t -> 'a t -> 'b t
  end
end

module Make1(Kernel: OBMonad.Kernel1): S1
  with type 'a t = 'a Kernel.t
= struct
  type 'a t = 'a Kernel.t

  module Core = struct
    let apply wrapped_f wrapped_x =
      Kernel.bind wrapped_f (fun f ->
        Kernel.bind wrapped_x (fun x ->
          Kernel.return (f x)))

    let map f a = Kernel.bind a (fun x -> Kernel.return (f x))

    let pure = Kernel.return
  end
  include Core

  module Infix = struct
    let (<$>) = map
    let (<*>) = apply
    let (>>|) a f = map f a
  end
end

module type S2 = sig
  type ('a, 'b) t

  module Core: sig
    val apply: ('a -> 'b, 'c) t -> ('a, 'c) t -> ('b, 'c) t
    val map: ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
    val pure: 'a -> ('a, _) t
  end
  include module type of Core

  module Infix: sig
    val (<$>): ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
    val (>>|): ('a, 'c) t -> ('a -> 'b) -> ('b, 'c) t
    val (<*>): ('a -> 'b, 'c) t -> ('a, 'c) t -> ('b, 'c) t
  end
end

module Make2(Kernel: OBMonad.Kernel2): S2
  with type ('a, 'b) t = ('a, 'b) Kernel.t
= struct
  type ('a, 'b) t = ('a, 'b) Kernel.t

  module Core = struct
    let apply wrapped_f wrapped_x =
      Kernel.bind wrapped_f (fun f ->
        Kernel.bind wrapped_x (fun x ->
          Kernel.return (f x)))

    let map f a = Kernel.bind a (fun x -> Kernel.return (f x))

    let pure = Kernel.return
  end
  include Core

  module Infix = struct
    let (<$>) = map
    let (<*>) = apply
    let (>>|) a f = map f a
  end
end
