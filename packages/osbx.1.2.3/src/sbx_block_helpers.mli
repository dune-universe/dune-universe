open Sbx_block

val block_to_filename     : Block.t        -> string option

val try_block_to_filename : Block.t option -> string option

val block_type_to_raw_header_pred : Block.block_type -> (Header.raw_header -> bool)
