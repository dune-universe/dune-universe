#if OCAML_VERSION >= (4, 07, 0)
  module%override Stdlib = struct
    module%override Lexing = struct
      type position = _ [@@rewrite] [@@deriving show]
    end
  end
#else
  module%override Lexing = struct
    type position = _ [@@rewrite] [@@deriving show]
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
  type toplevel_phrase and co [@@remove]

  [%%types] [@@deriving show]

  type toplevel_phrase = _ and co
end

let test () =
  let loc = Location.none in
  ignore (Parsetree.show_structure [%str ()])

let () = test ()
