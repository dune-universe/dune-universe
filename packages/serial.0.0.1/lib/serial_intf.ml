module type Serial_config_type = sig
  val port : string
end

module type Serial_type = sig 
  val port : string

  module Private : sig
    val fd : Lwt_unix.file_descr
    val in_channel : Lwt_io.input Lwt_io.channel
    val out_channel : Lwt_io.output Lwt_io.channel
  end

  val read_line : unit -> string Lwt.t
  val write_line : string -> unit Lwt.t

  val wait_for_line : string -> unit Lwt.t

  val io_loop : string option -> unit Lwt.t
end
