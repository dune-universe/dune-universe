(** Description of a type error *)

open Fmlib


type t =
  | Higher_universe of int
  | Not_a_type
  | Naming_no_type_variable
  | Naming_type_variable
  | Name_not_found of string
  | Wrong_type of Term.typ * Term.typ * Gamma.t
  | No_type_allowed of Term.typ * Gamma.t
  | Not_a_function of Term.typ * Gamma.t
  | Pi_not_a_function
  | Not_yet_implemented of string



module Print (P: Pretty_printer.SIG) =
struct
    module Term_print = Term_printer.Pretty (Gamma) (P)

    let print (error: t): P.t =
        let open P in
        match error with
        | Higher_universe uni ->
            string ("Universe level " ^ string_of_int uni ^ " not allowed")
            <+> cut

        | Not_a_type ->
            string "I have expected a type, but this is not a type."
            <+> cut

        | Naming_no_type_variable ->
            wrap_words
                "This identifier must not start with an upper case letter. \
                Identifiers starting with upper case letters are allowed \
                only for types and type constructors."
            <+> cut

        | Naming_type_variable ->
            wrap_words
                "This identifier must not start with a lower case letter. \
                Identifiers starting with lower case letters are allowed \
                only for object variables, proofs and propositions."
            <+> cut

        | Name_not_found name ->
            string ("Cannot find <" ^ name ^ ">") <+> cut

        | Wrong_type (req_typ, act_typ, gamma) ->
            wrap_words "I was expecting a term which has the type"
            <+> cut
            <+> nest 4 (cut <+> Term_print.print req_typ gamma)
            <+> cut <+> cut
            <+> wrap_words "but the highlighted term has the type"
            <+> cut
            <+> nest 4 (cut <+> Term_print.print act_typ gamma)
            <+> cut <+> cut

        | No_type_allowed (req_typ, gamma) ->
            wrap_words "I was expecting a term which has the type"
            <+> cut
            <+> nest 4 (cut <+> Term_print.print req_typ gamma)
            <+> cut <+> cut
            <+> wrap_words "Therefore a type is not allowed here"
            <+> cut <+> cut

        | Not_a_function (typ, gamma) ->
            wrap_words
                "I was expecting a function, because there are \
                more arguments to come. But this term is not a function. \
                It has the type"
            <+> cut
            <+> nest 4 (cut <+> Term_print.print typ gamma)
            <+> cut <+> cut

        | Pi_not_a_function ->
            wrap_words
                "I was expecting a function, because there are \
                more arguments to come. But this term is not a function."
            <+> cut

        | Not_yet_implemented what ->
            string (what ^ " is not yet implemented") <+> cut
end
