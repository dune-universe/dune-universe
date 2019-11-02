val max_segments_per_request : int

module FeatureIndirect : sig
  type t = { max_indirect_segments : int }

  val _max_indirect_segments : string

  val to_assoc_list : t -> (string * string) list
end

module State : sig
  type t = | Initialising | InitWait | Initialised | Connected | Closing | Closed
  val _state : string
  val of_string : string -> t option
  val to_assoc_list : t -> (string * string) list
  val to_string : t -> string
end

module Mode : sig
  type t = ReadOnly | ReadWrite
end

module Media : sig
  type t = CDROM | Disk
end

module DiskInfo : sig
  type t = {
    sector_size : int;
    sectors : int64;
    media : Media.t;
    mode : Mode.t;
  }

  val to_assoc_list : t -> (string * string) list
end

module Hotplug : sig
  val _hotplug_status : string
  val _online : string
end

module Connection : sig
  type t = {
    virtual_device: string;
    backend_path: string;
    backend_domid: int;
    frontend_path: string;
    frontend_domid: int;
    mode: Mode.t;
    media: Media.t;
    removable: bool;
  }

  val to_assoc_list : t -> (int * (string * string)) list
end

module Req : sig
  type seg = {
    gref : OS.Xen.Gntref.t;
    first_sector : int;
    last_sector : int;
  }

  type segs = | Direct of seg array | Indirect of int32 array

  type op = | Read | Write | Write_barrier | Flush | Op_reserved_1 | Trim | Indirect_op

  val string_of_op : op -> string

  type t = {
    op : op option;
    handle : int;
    id : int64;
    sector : int64;
    nr_segs : int;
    segs : segs;
  }

  val get_segments : Cstruct.t -> int -> seg array

  module type PROTOCOL_IMPLEMENTATION = sig
    val total_size : int
    val segments_per_indirect_page : int
    val write_segments : seg array -> Cstruct.t -> unit
    val write_request : t -> Cstruct.t -> int64
    val read_request : Cstruct.t -> t
  end

  module Proto_32 : PROTOCOL_IMPLEMENTATION
  module Proto_64 : PROTOCOL_IMPLEMENTATION

end

module Res : sig
  type rsp = | OK | Error | Not_supported

  type t = {
    op : Req.op option;
    st : rsp option;
  }

  val write_response : int64 * t -> Cstruct.t -> unit

  val read_response : Cstruct.t -> int64 * t
end

module Protocol : sig
  type t = X86_64 | X86_32 | Native
end

module RingInfo : sig
  type t = {
    ref : int32;
    event_channel : int;
    protocol : Protocol.t;
  }

  val keys : string list
  val to_string : t -> string
  val of_assoc_list : (string * string) list -> (t, [`Msg of string]) result
end
