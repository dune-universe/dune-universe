module type IO = Io.IO

module type Writer_monad = sig
  module IO : IO

  val json_writer
       : writer:(string -> unit IO.t)
      -> eol:string
      -> incr:int
      -> psep:string
      -> 'a Json_internal.constrained
      -> unit IO.t
  val write_json : writer:(string -> unit IO.t) -> 'a Json_internal.constrained -> unit IO.t
  val write_json_hum : writer:(string -> unit IO.t) -> 'a Json_internal.constrained -> unit IO.t
end

module Make (Compliance : Compliance.S) (IO : IO) : Writer_monad with module IO := IO
