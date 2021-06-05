(** JSON parsers for files, strings and more

    Jsonxt provides a number JSON parsers and writers with
    a focus on performance for the core file and string functions
*)

(**
    {1 Parsers}
    [Jsonxt] provides a number of different JSON parsers supporting
    various access methods and levels of compliance with the JSON standard.
    Access methods include
    - File and string parsing including [Stream.t] support
    - A stream parser, that delivers a stream of json tokens rather than a
      whole json tree, with [Stream.t] support.
    - a monadic parser

    JSON compliance has a number of levels
    - Extended supports floats and integers as well as tuples and variants from Yojson
    - Basic supports floats and integers
    - Strict only supports floats with integers converted to floats
    
    In addition Yojson compatability modules supports basic Yojson functions
    including inter-operability with [ppx_deriving_yojson] and [ppx_yojson_conv]

    {1 Writers}
    [Jsonxt] supports writers for each of the access methods and compliance levels
    including compact and human readable versions. This includes
    - File, string and channel output
    - Stream writer for a json token stream such as produced by the stream parser
    - A monadic writer
    
    {1 Modules}
    *)

module Json = Json
module Json_stream = Json_stream
module Basic = Basic
module Basic_stream = Basic_stream
module Basic_monad = Basic_monad
module Extended = Extended
module Extended_stream = Extended_stream
module Extended_monad = Extended_monad
module Strict = Strict
module Strict_stream = Strict_stream
module Strict_monad = Strict_monad
module Utilities = Utilities
module Error_info = Error_info
module Yojson = Yojson
