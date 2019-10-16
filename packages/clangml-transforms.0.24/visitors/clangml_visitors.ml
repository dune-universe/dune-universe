[@@@ocaml.warning "-30"]

[%%rewrite
  module%import Clang__ = struct
    module%import Clang__bindings = struct
      type cxcursor =
        Clang__.Clang__bindings.cxcursor [@opaque] [@@rewrite] [@@remove]
      type cxtype =
        Clang__.Clang__bindings.cxtype [@opaque] [@@rewrite] [@@remove]
      type cxcursorkind =
        Clang__.Clang__bindings.cxcursorkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_declkind =
        Clang__.Clang__bindings.clang_ext_declkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_typekind =
        Clang__.Clang__bindings.clang_ext_typekind [@opaque] [@@rewrite] [@@remove]
      type cxfloat =
        Clang__.Clang__bindings.cxfloat [@opaque] [@@rewrite] [@@remove]
      type cxint =
        Clang__.Clang__bindings.cxint [@opaque] [@@rewrite] [@@remove]
      type cxlinkagekind =
        Clang__.Clang__bindings.cxlinkagekind [@opaque] [@@rewrite] [@@remove]
      type cxcallingconv =
        Clang__.Clang__bindings.cxcallingconv [@opaque] [@@rewrite] [@@remove]
      type cx_cxxaccessspecifier =
        Clang__.Clang__bindings.cx_cxxaccessspecifier
            [@opaque] [@@rewrite] [@@remove]
      type cxtypekind =
        Clang__.Clang__bindings.cxtypekind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_attrkind =
        Clang__.Clang__bindings.clang_ext_attrkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_binaryoperatorkind =
        Clang__.Clang__bindings.clang_ext_binaryoperatorkind
            [@opaque] [@@rewrite] [@@remove]
      type clang_ext_unaryoperatorkind =
        Clang__.Clang__bindings.clang_ext_unaryoperatorkind
            [@opaque] [@@rewrite] [@@remove]
      type clang_ext_unaryexpr =
        Clang__.Clang__bindings.clang_ext_unaryexpr [@opaque] [@@rewrite] [@@remove]
      type clang_ext_characterkind =
        Clang__.Clang__bindings.clang_ext_characterkind [@opaque]
            [@@rewrite] [@@remove]
      type clang_ext_elaboratedtypekeyword =
        Clang__.Clang__bindings.clang_ext_elaboratedtypekeyword
            [@opaque] [@@rewrite] [@@remove]
      type cxsourcelocation =
        Clang__.Clang__bindings.cxsourcelocation [@opaque] [@@rewrite] [@@remove]
      type clang_ext_stmtkind =
        Clang__.Clang__bindings.clang_ext_stmtkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_lambdacapturekind =
        Clang__.Clang__bindings.clang_ext_lambdacapturekind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_lambdacapturedefault =
        Clang__.Clang__bindings.clang_ext_lambdacapturedefault [@opaque] [@@rewrite] [@@remove]
      type clang_ext_predefinedexpr_identkind =
        Clang__.Clang__bindings.clang_ext_predefinedexpr_identkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_stringkind =
        Clang__.Clang__bindings.clang_ext_stringkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_overloadedoperatorkind =
        Clang__.Clang__bindings.clang_ext_overloadedoperatorkind [@opaque] [@@rewrite] [@@remove]
      type clang_ext_exceptionspecificationtype =
        Clang__.Clang__bindings.clang_ext_exceptionspecificationtype [@opaque] [@@rewrite] [@@remove]
    end
    module%import Clang__types = struct
      type language = language [@opaque] [@@rewrite] [@@remove]
    end
  end

  module%import Clang = struct
    [%%recursive
     module%import Ast = struct
       type concrete_location = _
       type source_location = _
       type 'qual_type open_decoration = _
       type ('a, 'qual_type) open_node = _
     end]
     [@@deriving
        visitors { variety = "iter"; name = "base_iter"; polymorphic = true },
        visitors { variety = "map"; name = "base_map"; polymorphic = true },
        visitors {
          variety = "reduce"; name = "base_reduce"; polymorphic = true },
        visitors {
          variety = "mapreduce"; name = "base_mapreduce"; polymorphic = true }]

    [%%recursive
     module%import Types = struct
       type language = _ [@@rewrite]
     end
     module%import Ast = struct
       type translation_unit = _ and co
     end]
       [@@deriving
         visitors { variety = "iter"; ancestors = ["base_iter"] },
         visitors { variety = "map"; ancestors = ["base_map"] },
         visitors { variety = "reduce"; ancestors = ["base_reduce"] },
         visitors { variety = "mapreduce"; ancestors = ["base_mapreduce"] }]
  end]
