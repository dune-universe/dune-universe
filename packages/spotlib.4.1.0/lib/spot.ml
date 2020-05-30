include Base

module Monad      = Monad

module Option     = Option
include Option.Stdlib

module Weaktbl    = Weaktbl
(* module Phantom    = Phantom *)
module Hashset    = Hashset
module File       = File
module Comopt     = Comopt
module Overload   = Overload
module Mtypes     = Mtypes
module Stream     = SpotStream
module Poly_result = Poly_result
module Result     = Vresult
include Result.Stdlib

module Tuple      = Tuple
include Tuple.Stdlib

module Exn        = Exn
include Exn.Stdlib

module Temporal = Temporal
module Year = Temporal.Year
module Date = Temporal.Date
module Time = Temporal.Time
module Datetime = Temporal.Datetime

module IntRange = IntRange

module List = struct
  include List
  include Xlist
end
include Xlist.Stdlib

module Array = struct
  include Array
  include Xarray
end

module Format = struct
  include Format
  include Xformat
end

module Hashtbl = struct
  include Hashtbl
  include Xhashtbl
end

module String = struct
  include String
  include Xstring
end
include Xstring.Stdlib

module Bytes = struct
  include Bytes
  include Xbytes
end

module Lazy = struct
  include Lazy
  include Xlazy
end

module Filename = struct
  include Filename
  include Xfilename
end
include Xfilename.Stdlib

module Filepath = Filepath

module Unix = struct
  include Unix
  include Xunix
end
include Xunix.Stdlib
  
module Printf = struct
  include Printf
  include Xprintf
end

module Sys = struct
  include Sys
  include Xsys
end

module Set = Xset
module StringSet = Set.Make(String)
module IntSet = Set.Make(struct
  type t = int
  let compare (x:int) y = compare x y
end)

module Int64 = struct
  include Int64
  include Xint64
end

module Printexc = struct
  include Printexc
  include Xprintexc
end

module Obj = struct
  include Obj
  include Xobj
end

module URL = URL

module Gc = struct
  include Gc
  include Xgc
end

module UniqueID = UniqueID

module Once = Once

module Shell = Shell

module Lexing = struct
  include Lexing
  include Xlexing
end

module Command = Command
