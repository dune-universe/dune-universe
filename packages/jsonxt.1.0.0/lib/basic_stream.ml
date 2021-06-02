module Lexxer = Compliant_lexxer.Make(Basic.Compliance)
module Parser_stream = Parser_stream.Make(Basic.Compliance)
include Reader_stream.Make (Lexxer) (Parser_stream)
include Writer_stream.Make(Basic.Compliance)
