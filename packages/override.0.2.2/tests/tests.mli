module%override Base : sig
  type t = float

  type z = _ [@@from: u] [@@rewrite]

  type a = int

  module%override M : sig
    type 'a t = _ [@@deriving]
  end

  module%override F (X : S) : sig
    type t = X.t
  end

  module N : sig
    type t = int
  end

  module%override O : sig
    type u = float

    [%%types] [@@deriving]
  end

  module%override P : sig
    [%%recursive:
      type v [@@remove]
      [%%types] [@@deriving]]
  end

  module%override G (Y : S) : sig
    [%%types]
  end

  module%override Rec_group : sig
    type c = _ and co [@@deriving]

    type x = _ [@@from: a] and y = _ [@@from: b]
  end

  module%override Module_type : sig
    type first = bool
    [%%types]
  end

  module%override Redefine_module_type : sig
    module type%override S = sig
      type t

      val compare : t -> t -> int
    end
  end
end
