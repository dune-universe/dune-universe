[%%recursive:
#if OCAML_VERSION >= (4, 07, 0)
  module%import Stdlib : sig
    module%import Lexing : sig
      type position = _ [@@rewrite]
    end
  end
#else
  module%import Lexing : sig
    type position = _ [@@rewrite]
  end
#endif

  module%import Location : sig
    type location = _ [@@from: t] [@@rewrite]

    type 'a loc = _ [@@rewrite]
  end

  module%import Longident : sig
    type longident = _ [@@from: t] [@@rewrite]
  end

  type longident_loc = longident loc [@@rewrite]

  module%import Asttypes : sig
    type constant [@@remove]

    type 'a loc [@@rewrite] [@@remove]

    [%%types] [@@rewrite]
  end

  module%import Parsetree : sig
    type toplevel_phrase and co [@@remove]

    [%%types]
  end]
