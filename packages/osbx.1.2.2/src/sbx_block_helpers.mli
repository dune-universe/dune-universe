open Sbx_block

val block_to_filename     : Block.t        -> string option

val try_block_to_filename : Block.t option -> string option
