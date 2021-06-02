module type IO = Io.IO

module type Parser = sig
  module IO : IO
  module Compliance : Compliance.S

  val decode
    :  reader : (unit -> Tokens.token IO.t)
    -> (Compliance.json option, string) result IO.t
end

module Make (Compliance : Compliance.S) (IO : IO) : Parser
  with module IO = IO
   and module Compliance = Compliance
= struct

  module IO = IO
  module Compliance = Compliance

  open IO
  module Error_or = Error_or.Make(IO)
  open Error_or

  let json_value ~reader = 
    let open Tokens in
    let open Parser_tools in

    let rec token_value tok = begin
      match tok with
      | INT i -> return (Compliance.integer i)
      | STRING s -> return (Compliance.string s)
      | BOOL b -> return (Compliance.bool b)
      | FLOAT f -> return (Compliance.number (`Float f))
      | INFINITY -> return (Compliance.number `Infinity)
      | NEGINFINITY -> return (Compliance.number `Neginfinity)
      | NAN -> return (Compliance.number `Nan)
      | NULL -> return (Compliance.null)
      | LARGEINT s -> return (Compliance.largeint s)
      | EOF -> fail `Eof
      | COMMA | COLON | AE | OE | TE | VE | LEX_ERROR _ | COMPLIANCE_ERROR _ ->
        fail (token_error tok)
      | AS -> array_value_start ()
      | OS -> object_value_start ()
      | TS -> tuple_value_start ()
      | VS -> variant_value_start ()
    end
    and value () = begin
      reader () >>= fun tok -> token_value tok
    end
    and array_value_start () = begin
      reader () >>= fun tok -> 
      match tok with
      | AE -> return (Compliance.list [])
      | _ -> array_values_start tok []
    end
    and array_values_start tok acc = begin
      token_value tok >>=? fun v ->
      reader () >>= fun tok -> 
      match tok with
      | AE -> return (Compliance.list (List.rev (v::acc)))
      | COMMA -> array_values (v::acc)
      | tok -> fail (token_error tok)
    end
    and array_values acc = begin
      value () >>=? fun v ->
      reader () >>= fun tok -> 
      match tok with
      | AE -> return (Compliance.list (List.rev (v::acc)))
      | COMMA -> array_values (v::acc)
      | tok -> fail (token_error tok)
    end
    and object_value_start () = begin
      reader () >>= fun tok -> 
      match tok with
      | OE -> return (Compliance.assoc [])
      | _ -> object_values_start tok []
    end
    and object_values_start tok acc = begin
      colon_value tok () >>=? fun v ->
      reader () >>= fun tok -> 
      match tok with
      | OE -> return (Compliance.assoc (List.rev (v::acc)))
      | COMMA -> object_values (v::acc)
      | tok -> fail (token_error tok)
    end
    and object_values acc = begin
      key_colon_value () >>=? fun v ->
      reader () >>= fun tok -> 
      match tok with
      | OE -> return (Compliance.assoc (List.rev (v::acc)))
      | COMMA -> object_values (v::acc)
      | tok -> fail (token_error tok)
    end
    and colon_value v () = begin
      match v with
      | STRING k -> begin
        reader () >>= fun tok -> 
        match tok with
        | COLON ->
          value () >>=? fun v -> return (k, v)
        | tok -> fail (token_error tok)
        end
      | tok -> fail (token_error tok)
    end
    and key_colon_value () = begin
      reader () >>= fun tok -> 
      match tok with
      | STRING k -> begin
        reader () >>= fun tok -> 
        match tok with
        | COLON ->
          value () >>=? fun v -> return (k, v)
        | tok -> fail (token_error tok)
        end
      | tok -> fail (token_error tok)
    end
    and tuple_value_start () = begin
      value () >>=? fun v1 ->
      reader () >>= fun tok -> 
      match tok with
      | COMMA -> begin
        value () >>=? fun v2 ->
        reader () >>= fun tok -> 
        match tok with
        | TE -> return (Compliance.tuple [v1; v2])
        | COMMA -> tuple_values [v2; v1]
        | tok -> fail (token_error tok)
        end
      | TE -> fail (`Syntax_error "tuple must have at least 2 elements")
      | tok -> fail (token_error tok)
    end
    and tuple_values acc = begin
      value () >>=? fun v ->
      reader () >>= fun tok -> 
      match tok with
      | TE -> return (Compliance.tuple (List.rev (v::acc)))
      | COMMA -> tuple_values (v::acc)
      | tok -> fail (token_error tok)
    end
    and variant_value_start () = begin
      reader () >>= fun tok -> 
      match tok with
      | STRING k -> begin
        reader () >>= fun tok -> 
        match tok with
        | VE -> return (Compliance.variant k None)
        | COLON ->
          value () >>=? fun v -> variant_end k (Some v)
        | tok -> fail (token_error tok)
        end
      | VE -> fail (`Syntax_error "variant must have at least a string")
      | tok -> fail (token_error tok)
    end
    and variant_end k v = begin
      reader () >>= fun tok -> 
      match tok with
      | VE -> return (Compliance.variant k v)
      | tok -> fail (token_error tok)
    end
    in
    reader () >>= fun tok -> 
    match tok with
    | EOF -> return None
    | tok -> token_value tok >>=? fun res -> return (Some res)

  let decode ~reader = 
    json_value ~reader
    >>= function
    | Ok res -> return res
    | Error (`Syntax_error err) -> fail err
    | Error `Eof  -> fail "unexpected end-of-input"

end

