module type Kernel = sig
  type 'a t

  val add: 'a t -> 'a t -> 'a t
  val zero: 'a t
end

module type S = sig
  include Kernel

  module Infix: sig
    val (++): 'a t -> 'a t -> 'a t
  end
end

module Make(K: Kernel) = struct
  type 'a t = 'a K.t

  let add = K.add
  let zero = K.zero

  module Infix = struct
    let (++) = K.add
  end
end
