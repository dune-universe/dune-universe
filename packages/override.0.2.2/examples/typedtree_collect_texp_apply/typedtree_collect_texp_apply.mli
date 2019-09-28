type texp_apply =
    Typedtree.expression *
      (Asttypes.arg_label * Typedtree.expression option) list

val collect_texp_apply_from_structure
  : Typedtree.structure -> texp_apply list

val collect_texp_apply_from_binary_annots
  : Cmt_format.binary_annots -> texp_apply list
