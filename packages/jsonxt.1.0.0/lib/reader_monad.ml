module type Reader_monad = sig
  module IO : Io.IO
  type json

  val read_json : ?stream:bool -> reader:(Bytes.t -> int -> int IO.t) -> unit -> (json, string) result IO.t
end

module Make
    (Parser : Parser_monad.Parser)
  : Reader_monad with type json = Parser.Compliance.json and module IO := Parser.IO
= struct
  type json = Parser.Compliance.json

  open Parser.IO

  let create_lex_reader reader =
    let module Lexxer = Compliant_lexxer_monad.Make (Parser.Compliance)
        (struct
          module IO = Parser.IO
          include IO
          let read buf len = reader buf len
        end)
    in
    fun lexbuf -> Lexxer.read lexbuf

  let read_json ?(stream = false) ~reader () =
    let lexbuf = Lexutils.create_lexbuf () in
    let lex_reader = create_lex_reader reader in
    let reader () = 
      lex_reader lexbuf
      >>= function
      | Ok tok -> return tok
      | Error err -> return (Tokens.LEX_ERROR err)
    in
    Parser.decode ~reader
    >>= (function
    | Ok None -> return (Error "empty string")
    | Ok (Some res) -> begin
      match stream with
      | true -> return (Ok res)
      | false ->
        reader ()
        >>= function
        | EOF -> return (Ok res)
        | tok -> return (Error (("junk after end of JSON value: " ^ (Token_utils.token_to_string tok))))
      end
    | Error s -> return (Error s))
    >>= function
    | Ok _ as res -> return res
    | Error err ->
      let err_info = Error_info.create_from_lexbuf lexbuf err in
      return (Error (Error_info.to_string err_info))
end
