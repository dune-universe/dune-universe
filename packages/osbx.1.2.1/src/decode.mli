open Sbx_specs
open Sbx_block
open Stream_file

module Stats : sig
  type t = { block_size            : int
           ; blocks_processed      : int64
           ; meta_blocks_decoded   : int64
           ; data_blocks_decoded   : int64
           ; blocks_failed         : int64
           ; failed_block_pos_list : int64 list
           ; recorded_hash         : Multihash.hash_bytes option
           ; output_file_hash      : Multihash.hash_bytes option
           ; start_time            : float
           }

  val print_stats : t -> unit
end

type stats = Stats.t

module Process : sig
  val fetch_ref_block : in_filename:string -> (Block.t option, string) result

  val decode_file     : ref_block:Block.t option -> in_filename:string -> out_filename:string option -> (stats, string)         result
end
