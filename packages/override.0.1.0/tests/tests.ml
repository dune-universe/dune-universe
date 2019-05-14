module%override Base : Tests_spec.S = struct
  type t = float

  type z = _ [@@from: u] [@@rewrite]

  type a = int

  module%override M = struct
    type 'a t = _ [@@deriving]
  end

  module%override F (X : S) = struct
    type t = X.t
  end

  module N = struct
    type t = int
  end

  module%override O = struct
    type u = float

    [%%types] [@@deriving]
  end

  module%override P = struct
    [%%recursive
      type v [@@remove]
      [%%types] [@@deriving]]
  end

  module%override G (Y : S) = struct
    [%%types]
  end

  module%override Rec_group = struct
    type c = _ and co [@@deriving]
  end

  module%override Module_type = struct
    type first = bool
    [%%types]
  end

  module%override Redefine_module_type = struct
    module type%override S = sig
      type t

      val compare : t -> t -> int
    end
  end
end
