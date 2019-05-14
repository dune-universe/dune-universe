module type S = sig
  type t = float

  type z = Base.u = A | B

  type a = int

  val x : int

  module M : sig
    type 'a t = 'a option

    val x : int
  end

  module type S = sig
    type t
  end

  module F (X : S) : sig
    type t = X.t
  end

  module N : sig
    type t = int
  end

  module O : sig
    type t = int

    type u = float

    type 'a v = 'a option
  end

  module P : sig
    type t = Base.P.t = A of z

    type u = bool
  end

  module G (Y : S) : sig
    type t = Y.t
  end

  module Rec_group : sig
    type a = Base.Rec_group.a = A of b and b = Base.Rec_group.b = B of a
    type c = Base.Rec_group.c = C of d and d = Base.Rec_group.d = D of c
  end

  module Module_type : sig
    type first = bool
    module type S = sig type t = first end
    type last = (module S)
  end

  module Redefine_module_type : sig
    val x : unit

    module type S = sig
      type t

      val compare : t -> t -> int
    end
  end
end
