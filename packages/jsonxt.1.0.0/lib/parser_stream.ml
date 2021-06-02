module type Parser = sig
  module Compliance : Compliance.S
  type t

  val create :  reader : (unit -> Tokens.token) -> t
  val decode : t -> (Compliance.json_stream option, string) result
end

module Make (Compliance : Compliance.S) : Parser
  with module Compliance = Compliance
= struct

  module Compliance = Compliance

  exception Parse_error of [`Eof | `Syntax_error of string]

  type t = {
    reader : unit -> Tokens.token
  ; continuation : (unit -> Compliance.json_stream) Stack.t
  ; state : [`Start | `Process | `End] ref
  }

  let create ~reader = 
    { 
      reader
    ; continuation = Stack.create ()
    ; state = ref `Start
    }

  let json_stream t =
    let open Tokens in
    let open Parser_tools in
    let rec token_value tok = begin
      match tok with
      | INT i -> Compliance.Stream.integer i
      | STRING s -> Compliance.Stream.string s
      | BOOL b -> Compliance.Stream.bool b
      | FLOAT f -> Compliance.Stream.number (`Float f)
      | INFINITY -> Compliance.Stream.number `Infinity
      | NEGINFINITY -> Compliance.Stream.number `Neginfinity
      | NAN -> Compliance.Stream.number `Nan
      | NULL -> Compliance.Stream.null
      | LARGEINT s -> Compliance.Stream.largeint s
      | EOF -> raise (Parse_error `Eof)
      | COMMA | COLON | AE | OE | TE | VE | LEX_ERROR _ | COMPLIANCE_ERROR _ ->
        raise (Parse_error (token_error tok))
      | AS -> Stack.push array_value t.continuation; Compliance.Stream.array_start ()
      | OS -> Stack.push object_value t.continuation; Compliance.Stream.object_start ()
      | TS -> Stack.push tuple_value t.continuation; Compliance.Stream.tuple_start ()
      | VS -> Stack.push variant_value t.continuation; Compliance.Stream.variant_start ()
    end
    and array_value () = begin
      let tok = t.reader () in
      match tok with
      | AE -> Compliance.Stream.array_end ()
      | tok -> Stack.push array_value_next t.continuation; token_value tok
    end
    and array_value_next () = begin
      match t.reader () with
      | AE -> Compliance.Stream.array_end ()
      | COMMA ->
        let tok = t.reader () in
        Stack.push array_value_next t.continuation;
        token_value tok
      | tok -> raise (Parse_error (token_error tok))
    end
    and object_value () = begin
      let tok = t.reader () in
      match tok with
      | OE -> Compliance.Stream.object_end ()
      | STRING s -> Stack.push object_colon_value t.continuation; Compliance.Stream.name s
      | tok -> raise (Parse_error (token_error tok))
    end
    and object_colon_value () = begin
      match t.reader () with
      | COLON ->
        let tok = t.reader () in
        Stack.push object_value_next t.continuation;
        token_value tok
      | tok -> raise (Parse_error (token_error tok))
    end
    and object_value_next () = begin
      match t.reader () with
      | OE -> Compliance.Stream.object_end ()
      | COMMA -> begin
        match t.reader () with
        | STRING s -> Stack.push object_colon_value t.continuation; Compliance.Stream.name s
        | tok -> raise (Parse_error (token_error tok))
        end
      | tok -> raise (Parse_error (token_error tok))
    end
    and tuple_value () = begin
      let tok = t.reader () in
      match tok with
      | TE -> raise (Parse_error (`Syntax_error "tuple must have at least 2 elements"))
      | tok -> Stack.push tuple_value_1 t.continuation; token_value tok
    end
    and tuple_value_1 () = begin
      match t.reader () with
      | TE -> raise (Parse_error (`Syntax_error "tuple must have at least 2 elements"))
      | COMMA ->
        let tok = t.reader () in
        Stack.push tuple_value_2 t.continuation;
        token_value tok
      | tok -> raise (Parse_error (token_error tok))
    end
    and tuple_value_2 () = begin
      match t.reader () with
      | TE -> Compliance.Stream.tuple_end ()
      | COMMA ->
        let tok = t.reader () in
        Stack.push tuple_value_2 t.continuation;
        token_value tok
      | tok -> raise (Parse_error (token_error tok))
    end
    and variant_value () = begin
      match t.reader () with
      | STRING s -> Stack.push variant_colon_or_end t.continuation; Compliance.Stream.name s
      | tok -> raise (Parse_error (token_error tok))
    end
    and variant_colon_or_end () = begin
      match t.reader () with
      | VE -> Compliance.Stream.variant_end ()
      | COLON ->
        let tok = t.reader () in
        Stack.push variant_end t.continuation;
        token_value tok
      | tok -> raise (Parse_error (token_error tok))
    end
    and variant_end () = begin
      match t.reader () with
      | VE -> Compliance.Stream.variant_end ()
      | tok -> raise (Parse_error (token_error tok))
    end
    in
    if Stack.is_empty t.continuation then begin
      match t.reader () with
      | exception (Parse_error `Eof) -> None
      | exception exn_ -> raise exn_
      | tok -> begin
          match tok with
          | EOF -> None
          | tok -> Some (token_value tok)
        end
    end
    else Some ((Stack.pop t.continuation) ())

  let decode t = 
    let handle_eof () =
      if Stack.length t.continuation > 0 then Error "unexpected end-of-input"
      else begin
        match !(t.state) with
        | `Start -> Error "empty input"
        | `Process
        | `End -> Ok None
      end
    in
    match json_stream t with
    | exception (Parse_error (`Syntax_error err)) -> Error err
    | exception (Lexxer_utils.Lex_error err) -> Error err
    | exception (Parse_error `Eof) -> handle_eof ()
    | None -> handle_eof ()
    | res ->
      match !(t.state) with
      | `Start -> t.state := `Process; Ok res
      | `Process -> if Stack.length t.continuation = 0 then t.state := `End; Ok res
      | `End -> Error "Junk following JSON value"

end
