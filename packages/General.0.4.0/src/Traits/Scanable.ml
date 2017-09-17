(* [ToList.|ToArray.|]scan[|_short][|_right][|_i|_acc] *)

(* @feature [try_|]scan_reduce[...] (like [try_|]reduce[...], but returning the intermediate results) *)

#include "Scanable.signatures.ml"

module ToContainer(C: sig type 'a t end) = struct
  #include "Scanable.signatures.ToContainer.ml"
end

module ToList = ToContainer(struct type 'a t = 'a list end)

module ToArray = ToContainer(struct type 'a t = 'a array end)

module Right = struct
  #include "Scanable.signatures.Right.ml"

  module ToContainer(C: sig type 'a t end) = struct
    #include "Scanable.signatures.Right.ToContainer.ml"
  end

  module ToList = ToContainer(struct type 'a t = 'a list end)

  module ToArray = ToContainer(struct type 'a t = 'a array end)
end

module Short = struct
  #include "Scanable.signatures.Short.ml"

  module ToContainer(C: sig type 'a t end) = struct
    #include "Scanable.signatures.Short.ToContainer.ml"
  end

  module ToList = ToContainer(struct type 'a t = 'a list end)

  module ToArray = ToContainer(struct type 'a t = 'a array end)

  module Right = struct
    #include "Scanable.signatures.Short.Right.ml"

    module ToContainer(C: sig type 'a t end) = struct
      #include "Scanable.signatures.Short.Right.ToContainer.ml"
    end

    module ToList = ToContainer(struct type 'a t = 'a list end)

    module ToArray = ToContainer(struct type 'a t = 'a array end)
  end
end
