module type Seed = sig
  type t
  val op : t -> t -> t
end

module type S = sig
  include Seed
  val ( * ) : t -> t -> t
  val concat : t NonEmptyList.t ->  t
end

module type Seed1 = sig
  type 'a t
  val op : 'a t -> 'a t -> 'a t
end

module type S1 = sig
  include Seed1
  val ( * ) : 'a t -> 'a t -> 'a t
  val concat : 'a t NonEmptyList.t -> 'a t
end

module Law (S : S) = struct
  let associativity x y z = S.(x * (y * z)) = S.((x * y) * z)
end

module Make (S:Seed) : S with type t = S.t = struct
  include S
  let ( * ) a b = op a b

  let concat xs = NonEmptyList.fold op xs

  (* TODO repeated apply *)
end

module Make1 (Seed: Seed1) : S1 with type 'a t = 'a Seed.t = struct
  include Seed
  let ( * ) a b = op a b
  let concat xs = NonEmptyList.fold op xs
end

let make (type a) op =
  let module Seed = (struct
    type t = a
    let op = op
  end)
  in
  (module Make (Seed) : S with type t = a)

(* Implementations *)

module Bool = struct
  module Or = (val make (||))
  module And = (val make (&&))
end

module Int = struct
  module Sum = (val make (+))
  module Product = (val make ( * ))
end

module Option = struct
  module Make (S : S) : S with type t = S.t Option.t = struct
    module Seed = struct
      type t = S.t Option.t
      let op a b =
        match a  , b with
        | None   , b      -> b
        | a      , None   -> a
        | Some a , Some b -> Some (S.op a b)
    end

    include Make (Seed)
  end
end

module Endo = struct
  module Make (T : Triv.S) : S with type t = (T.t -> T.t) = struct
    let compose : (T.t -> T.t) -> (T.t -> T.t) -> (T.t -> T.t) =
      fun f g x -> f (g x)
    include (val make compose)
  end

  let make (type a) (proxy : a Util.proxy) =
    (module Make (val Triv.make proxy) : S with type t = a -> a)
end

module Dual = struct
  module Make (S : S) : S with type t = S.t = struct
    include S
    let op = Fun.flip op
  end

  let make op = make (Fun.flip op)
end

module NonEmptyList = struct
  include NonEmptyList
  include Make1 (NonEmptyList)
end
