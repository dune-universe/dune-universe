open F

(* A type-checker for System F. *)

(* [typeof t] type-checks the closed term [t] and constructs its type. *)

exception NotAnArrow   of debruijn_type
exception NotAForall   of debruijn_type
exception TypeMismatch of debruijn_type * debruijn_type

val typeof: debruijn_term -> debruijn_type

