val convert_attribute : Parsetree.attribute -> Compat_types.attribute

val convert_signature_item : Types.signature_item -> Compat_types.signature_item

val alias_of_module_type : Types.module_type -> Path.t option
