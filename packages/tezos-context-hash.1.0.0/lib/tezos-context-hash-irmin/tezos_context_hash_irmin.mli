module Encoding = Encoding
open Encoding
module Conf : Irmin_pack.Conf.S

module Store : sig
  include
    Irmin.S
      with type key = Path.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type Private.Sync.endpoint = unit

  val reconstruct_index : ?output:string -> Irmin.config -> unit
end
