module%override Migrate_parsetree = struct
  module%override OCaml_408 = struct
    module%override Ast = struct
      module%override Longident = struct
        [%%recursive [%%types]] [@@deriving eq]
      end

      [%%rewrite_module Migrate_parsetree__Ast_408.Longident = Longident]

      module%override Location = struct
        let equal _ _ = true

        type 'a loc = _ [@@deriving eq]
      end

      [%%rewrite_module Migrate_parsetree__Ast_408.Location = Location]

      module%override Asttypes = struct
        type 'a loc [@@rewrite] [@@remove]

        type constant [@@remove]

        [%%recursive [%%types]] [@@deriving eq]
      end

      [%%rewrite_module Migrate_parsetree__Ast_408.Asttypes = Asttypes]

      module type S = sig
        val equiv_core_type :
            Parsetree.core_type -> Parsetree.core_type -> bool
      end

      module Make (X: S) = struct
        module%include Parsetree = struct
          type rec_type = Parsetree.core_type

          let equal_rec_type = X.equiv_core_type

          type core_type = rec_type [@@rewrite] [@@remove]

          type toplevel_phrase and co [@@remove]

          [%%recursive [%%types]] [@@deriving eq]

          type core_type = _ [@@deriving eq]
        end[@@ocaml.warning "-32"]
      end
    end
  end
end

let equal_loc = Migrate_parsetree.OCaml_408.Ast.Location.equal_loc

let equiv_core_type equiv t0 t1 =
  let module Equiv =
    Migrate_parsetree.OCaml_408.Ast.Make
      (struct let equiv_core_type = equiv end) in
  Equiv.equal_core_type t0 t1
