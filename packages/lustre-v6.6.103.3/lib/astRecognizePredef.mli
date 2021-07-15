(* Time-stamp: <modified the 26/02/2015 (at 13:44) by Erwan Jahier> *)

(** Replaces idref that corresponds to predefined items with the
    AstCore.Predef constructor

    Performs a (tedious) recursive traversal of the syntax tree.

    TODO : Solves idref references 

    - To do that, first paramatrize the AS (cf AstCore) by the kind
    of ident that is used.  Indeed, during parsing, we cannot
    always know what Lv6Id.long should we have, given an
    Lv6Id.idref, or a Lv6Id.t. The idea is then to write a function
    resolve_name which profile is

    (Lv6Id.idref) AstCore.t -> (long) AstCore.t
*)

val f : AstV6.t -> AstV6.t
