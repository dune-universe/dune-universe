(* Specification *)

module type Seed = sig
  include Semigroup.Seed
  val unit : t
end

module type S = sig

  (* Properly speaking, we could include the [Seed], but that obfuscates the
     docs, and this signature is equivalent *)

  include Semigroup.S
  val unit : t

  val mconcat : t list -> t
end


(* Laws *)

module Law (M : S) = struct

  include Semigroup.Law (M)

  let unit_right_cancelation x = M.(x * unit) = x

  let unit_left_cancelation x = M.(unit * x) = x

  let mconcat_is_a_fold_right xs = (M.mconcat xs) = List.fold_right M.op xs M.unit
end


(* Constructors *)

module Make (Seed : Seed) : S with type t = Seed.t = struct
  include Seed
  include Semigroup.Make (Seed)

  let mconcat t = List.fold_left op unit t
end

let make (type a) unit op =
  let module Seed = (struct
    type t = a
    let unit = unit
    let op = op
  end : Seed with type t = a)
  in
  (module Make (Seed) : S with type t = a)

let of_semigroup (type a) (module S : Semigroup.S with type t = a) unit =
  let module Seed = (struct
    include S
    let unit = unit
  end : Seed with type t = a)
  in
  (module Make (Seed) : S with type t = a)


(* Implementations *)

module Bool = struct
  module Or = (val of_semigroup (module Semigroup.Bool.Or) false)
  module And = (val of_semigroup (module Semigroup.Bool.And) true)
end

module Int = struct
  module Sum = (val of_semigroup (module Semigroup.Int.Sum) 0)
  module Product = (val of_semigroup (module Semigroup.Int.Product) 1)
end

module Option = struct
  module Make (S : Semigroup.S) = struct
    module Semi = Semigroup.Option.Make (S)
    include (val of_semigroup (module Semi) None)
  end
end

module Endo = struct
  module Make (T : Triv.S) : S with type t = (T.t -> T.t) = struct
    module Semi = Semigroup.Endo.Make (T)
    include (val of_semigroup (module Semi) (Fun.id : T.t -> T.t))
  end

  let make (type a) (proxy : a Util.proxy) =
    let semi = Semigroup.Endo.make proxy in
    of_semigroup semi (Fun.id : a -> a)
end

module Dual = struct
  module Make (M : S) : S with type t = M.t = struct
    module Semi_dual = Semigroup.Dual.Make (M)
    include (val of_semigroup (module Semi_dual) M.unit)
  end

  let dualize (type a) (module M : S with type t = a) =
    (module Make (M) : S with type t = a)
end
