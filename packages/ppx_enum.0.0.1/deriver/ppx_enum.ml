let enum =
  Ppxlib.Deriving.add
    "enum"
    ~str_type_decl:Ppx_enum_lib.Enum.from_str_type_decl
    ~sig_type_decl:Ppx_enum_lib.Enum.from_sig_type_decl
