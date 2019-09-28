module%import Self_import = struct
  [%%types]
end

let x = A

module Make (X : S) = struct
  module%import Self_import = struct
    module%import Make (X : S) = struct
      [%%types]
    end
  end

  let perform_twice t =
    X.perform t;
    X.perform t
end
