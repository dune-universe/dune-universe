#if OCAML_VERSION >= (4, 07, 0)
  module%override Stdlib = struct
    module%override Lexing = struct
      type position = _ [@@deriving show]
    end
  end
#else
  module%override Lexing = struct
    type position = _ [@@deriving show]
  end
#endif

module%override Longident = struct
  [%%types] [@@deriving show]
end

module%override Location = struct
  [%%types] [@@deriving show]
end

module%override Asttypes = struct
  [%%types] [@@deriving show]
end

module%override Parsetree = struct
  [%%types] [@@deriving show]
end
