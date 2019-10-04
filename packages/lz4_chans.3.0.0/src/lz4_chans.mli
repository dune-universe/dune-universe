
(** open/close binary channels, with LZ4-compression
    happening in the background, using a separate process and a named pipes *)

val open_in_bin: string -> in_channel
val open_out_bin: string -> out_channel

val close_in: in_channel -> unit
val close_out: out_channel -> unit

val with_in_file: string -> (in_channel -> 'a) -> 'a
val with_out_file: string -> (out_channel -> 'a) -> 'a
