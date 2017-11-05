open Sbx_block
open Stream_file
open Sbx_specs

exception File_metadata_get_failed

module Stats : sig
  type t = { block_size          : int
           ; data_size           : int
           ; blocks_written      : int64
           ; meta_blocks_written : int64
           ; data_blocks_written : int64
           ; total_data_encoded  : int64
           ; start_time          : float 
           }

  val print_stats : t -> unit
end

type stats = Stats.t

module Process : sig
  val encode_file : uid:string option -> want_meta:bool -> ver:version -> hash:string -> in_filename:string -> out_filename:string -> (stats, string) result
end
