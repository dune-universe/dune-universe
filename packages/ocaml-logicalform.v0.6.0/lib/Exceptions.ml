exception Invalid_literal of string

let invalid_sexp_exception ~(ctx : string) (sexp : Base.Sexp.t) : exn =
  Invalid_literal (Printf.sprintf "The S-Expression\n  %s\n is not a valid %s."
                                  (Base.Sexp.to_string_hum ~indent:4 sexp) ctx)