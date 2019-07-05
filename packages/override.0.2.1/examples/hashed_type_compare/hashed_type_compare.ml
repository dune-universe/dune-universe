module%override Hashtbl = struct
  module type HashedType = sig
    type t

    val compare : t -> t -> int

    val hash : t -> int
  end

  module Make (X : HashedType) = struct
    include Hashtbl.Make (struct
      type t = X.t

      let equal x y =
        X.compare x y = 0

      let hash = X.hash
    end)
  end
end
