exception Packaged_exn of string

module Stream : sig
  type 'a in_out_processor = in_channel  -> out_channel -> 'a

  type 'a in_processor     = in_channel  -> 'a

  type 'a out_processor    = out_channel -> 'a

  val process_in_out :
    ?pack_break_into_error:bool -> append:bool -> in_filename:string -> out_filename:string -> ('a in_out_processor)
    -> ('a, string) result

  val process_in  :
    ?pack_break_into_error:bool -> in_filename:string  -> ('a in_processor)
    -> ('a, string) result

  val process_out :
    ?pack_break_into_error:bool -> append:bool -> out_filename:string -> ('a out_processor)
    -> ('a, string) result
end

(* General helpers *)
module General_helper : sig
  exception Invalid_range

  val make_buffer            : int        -> bytes

  val get_from_buf           : buf:string -> pos:int      -> len:int        -> string 

  (* Inclusive range *)
  val get_from_buf_inc_range : buf:string -> start_at:int -> end_at:int     -> string

  (* Exclusive range *)
  val get_from_buf_exc_range : buf:string -> start_at:int -> end_before:int -> string
end

(* Helpers for reading into buffer *)
module Read_into_buf : sig
  exception Invalid_offset
  exception Invalid_length

  type read_stats  = { read_count : int }
  type read_result = read_stats option

  val read : ?offset:int -> ?len:int -> in_channel -> buf:bytes -> read_result
end

(* Helpers for reading and returning data as value *)
module Read_chunk : sig
  type read_content = { chunk : string }
  type read_result  = read_content option

  val read : in_channel -> len:int -> read_result
end

(* Helpers for writing from buffer *)
module Write_from_buf : sig
  exception Invalid_offset
  exception Invalid_length

  val write : ?offset:int -> ?len:int -> out_channel -> buf:bytes -> unit
end

module Write_chunk : sig
  val write : out_channel -> chunk:string -> unit
end
