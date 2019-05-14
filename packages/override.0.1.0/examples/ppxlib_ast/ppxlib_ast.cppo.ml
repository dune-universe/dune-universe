(* This example implements mechanically the transformation rules
   described in the comments of
   https://github.com/ocaml-ppx/ppxlib/blob/master/ast/ast.ml *)

(* "- replacing app [type ...] by [and ...] to make everything one
recursive block" *)
[%%recursive

  (* "- adding the type definitions for position, location, loc and longident"
     They are imported from Stdlib.Lexing.position, Location.t, 'a Location.loc,
     and Longident.t respectively. *)

  (* "- flattening all the modules"
     All the modules are imported with module%import. *)
#if OCAML_VERSION >= (4, 07, 0)
  module%import Stdlib = struct
    module%import Lexing = struct
      type position = _ [@@rewrite]
    end
  end
#else
  module%import Lexing = struct
    type position = _ [@@rewrite]
  end
#endif

  module%import Location = struct
    (* "- renaming a few types:
        - - Location.t -> location" *)
    type location = _ [@@from: t] [@@rewrite]

    type 'a loc = _ [@@rewrite]
  end

  module%import Longident = struct
    (* "- renaming a few types:
        - - Longident.t -> longident" *)
    type longident = _ [@@from: t] [@@rewrite]
  end

  (* "- adding a type longident_loc = longident loc and replacing all
   the occurences of the latter by the former. This is so that we can
   override iteration an the level of a longident loc." *)
  type longident_loc = longident loc [@@rewrite]

  module%import Asttypes = struct
    (* "- removing Asttypes.constant (unused and conflicts with
       Parsetree.constant)" *)
    type constant [@@remove]

    type 'a loc [@@rewrite] [@@remove]

    [%%types] [@@rewrite]
  end

  module%import Parsetree = struct
    type toplevel_phrase and co [@@remove]

    [%%types]
  end]
       [@@deriving eq]

let test () =
  let loc = Location.none in
  assert (equal_structure [%str (1, 2)] [%str (1, 2)]);
  assert (not (equal_structure [%str (1, "a")] [%str (1, "b")]))

let () = test ()
