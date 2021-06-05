module type Parser = sig
  module Compliance : Compliance.S

  val decode
    :  reader : (unit -> Tokens.token)
    -> (Compliance.json option, string) result
end

module Make (Compliance : Compliance.S) : Parser
  with module Compliance = Compliance
= struct

  module Compliance = Compliance

  exception Parse_error of [`Eof | `Syntax_error of string]

  let json_value ~reader = 
    let open Tokens in
    let open Parser_tools in
    let rec token_value tok = begin
      match tok with
      | INT i -> Compliance.integer i
      | STRING s -> Compliance.string s
      | BOOL b -> Compliance.bool b
      | FLOAT f -> Compliance.number (`Float f)
      | INFINITY -> Compliance.number `Infinity
      | NEGINFINITY -> Compliance.number `Neginfinity
      | NAN -> Compliance.number `Nan
      | NULL -> Compliance.null
      | LARGEINT s -> Compliance.largeint s
      | EOF -> raise (Parse_error `Eof)
      | COMMA | COLON | AE | OE | TE | VE | LEX_ERROR _ | COMPLIANCE_ERROR _ ->
        raise (Parse_error (token_error tok))
      | AS -> array_value_start ()
      | OS -> object_value_start ()
      | TS -> tuple_value_start ()
      | VS -> variant_value_start ()
    end
    and value () = token_value (reader ())
    and array_value_start () = begin
      let tok = reader () in
      match tok with
      | AE -> Compliance.list []
      | _ -> array_values_start tok []
    end
    and array_values_start tok acc = begin
      let v = token_value tok in
      match reader () with
      | AE -> Compliance.list (List.rev (v::acc))
      | COMMA -> array_values (v::acc)
      | tok -> raise (Parse_error (token_error tok))
    end
    and array_values acc = begin
      let v = value () in
      match reader () with
      | AE -> Compliance.list (List.rev (v::acc))
      | COMMA -> array_values (v::acc)
      | tok -> raise (Parse_error (token_error tok))
    end
    and object_value_start () = begin
      let tok = reader () in
      match tok with
      | OE -> Compliance.assoc []
      | _ -> object_values_start tok []
    end
    and object_values_start tok acc = begin
      let v = colon_value tok () in
      match reader () with
      | OE -> Compliance.assoc (List.rev (v::acc))
      | COMMA -> object_values (v::acc)
      | tok -> raise (Parse_error (token_error tok))
    end
    and object_values acc = begin
      let v = key_colon_value () in
      match reader () with
      | OE -> Compliance.assoc (List.rev (v::acc))
      | COMMA -> object_values (v::acc)
      | tok -> raise (Parse_error (token_error tok))
    end
    and colon_value v () = begin
      match v with
      | STRING k -> begin
        match reader () with
        | COLON -> (k, value ())
        | tok -> raise (Parse_error (token_error tok))
        end
      | tok ->  raise (Parse_error (token_error tok))
    end
    and key_colon_value () = begin
      match reader () with
      | STRING k -> begin
        match reader () with
        | COLON -> (k, value ())
        | tok -> raise (Parse_error (token_error tok))
        end
      | tok ->  raise (Parse_error (token_error tok))
    end
    and tuple_value_start () = begin
      let v1 = value () in
      match reader () with
      | COMMA -> begin
        let v2 = value () in
        match reader () with
        | TE -> Compliance.tuple [v1; v2]
        | COMMA -> tuple_values [v2; v1]
        | tok -> raise (Parse_error (token_error tok))
        end
      | TE -> raise (Parse_error (`Syntax_error "tuple must have at least 2 elements"))
      | tok -> raise (Parse_error (token_error tok))
    end
    and tuple_values acc = begin
      let v = value () in
      match reader () with
      | TE -> Compliance.tuple (List.rev (v::acc))
      | COMMA -> tuple_values (v::acc)
      | tok -> raise (Parse_error (token_error tok))
    end
    and variant_value_start () = begin
      match reader () with
      | STRING k -> begin
        match reader () with
        | VE -> Compliance.variant k None
        | COLON -> variant_end k (Some (value ()))
        | tok -> raise (Parse_error (token_error tok))
        end
      | VE -> raise (Parse_error (`Syntax_error "variant must have at least a string"))
      | tok -> raise (Parse_error (token_error tok))
    end
    and variant_end k v = begin
      match reader () with
      | VE -> Compliance.variant k v
      | tok -> raise (Parse_error (token_error tok))
    end
    in
    match reader () with
    | exception (Parse_error `Eof) -> None
    | exception exn_ -> raise exn_
    | EOF -> None
    | tok -> Some (token_value tok)

  let decode ~reader = 
    match json_value ~reader with
    | exception (Parse_error `Eof) -> Error "Unexpected end-of-input"
    | exception (Parse_error (`Syntax_error err)) -> Error err
    | exception (Lexxer_utils.Lex_error err) -> Error err
    | res -> Ok res

end
