module Lexxer = Compliant_lexxer.Make(Extended.Compliance)
module Parser_stream = Parser_stream.Make(Extended.Compliance)
include Reader_stream.Make (Lexxer) (Parser_stream)
include Writer_stream.Make(Extended.Compliance)
