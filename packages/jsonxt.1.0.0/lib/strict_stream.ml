module Lexxer = Compliant_lexxer.Make(Strict.Compliance)
module Parser_stream = Parser_stream.Make(Strict.Compliance)
include Reader_stream.Make (Lexxer) (Parser_stream)
include Writer_stream.Make(Strict.Compliance)
