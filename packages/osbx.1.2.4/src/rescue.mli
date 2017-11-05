open Stream_file
open Sbx_block

module Stats : sig
  type t = { bytes_processed       : int64
           ; blocks_processed      : int64
           ; meta_blocks_processed : int64
           ; data_blocks_processed : int64
           ; start_time            : float
           }

  val print_stats : t -> unit
end

type stats = Stats.t

module Process : sig
  val rescue_from_file :
    only_pick_block:Block.block_type ->
    only_pick_uid:string option ->
    from_byte:int64 option ->
    to_byte:int64 option ->
    force_misalign:bool ->
    in_filename:string ->
    out_dirname:string ->
    log_filename:string option ->
    (stats, string) result
end
