module type Reader_monad = sig
  module IO : Io.IO
  type json

  (** [read_json] takes a [reader] function and returns a [json] value or an error
      if the string has syntax, grammar or compliance errors.  The [reader buf len]
      parameter reads at most [len] bytes into [buf] and returns the number of
      bytes read. Zero indicates end of file. The optional [stream] parameter
      specifies if multiple [json] objects are to be read, defaulting to
      [false]
      *)
  val read_json : ?stream:bool -> reader:(Bytes.t -> int -> int IO.t) -> unit -> (json, string) result IO.t
end

module Make  (Parser : Parser_monad.Parser) : Reader_monad
  with type json = Parser.Compliance.json
  and module IO := Parser.IO

