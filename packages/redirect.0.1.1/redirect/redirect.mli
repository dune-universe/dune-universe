val read_and_close : in_channel -> (unit -> 'a) -> 'a
(** [read_and_close chan f] evaluates [f ()], then closes [chan]
    and returns the value computed by the evaluation of [f].
    [chan] is close even if [f] raises an exception.
    The difference between [read_and_close chan f] and
    [Fun.protect f ~finally:(fun () -> close_in chan)]
    is that if [f ()] succeeds and [close_in chan] raises an exception,
    this exception is not intercepted, and if [f ()] raises an exception,
    then [close_in_noerr] is used so that the exception raised by [f ()]
    is not masked.
*)

val write_and_close : out_channel -> (unit -> 'a) -> 'a
(** [write_and_close chan f] evaluates [f ()], then closes [chan]
    and returns the value computed by the evaluation of [f].
    [chan] is close even if [f] raises an exception.
    The difference between [write_and_close chan f] and
    [Fun.protect f ~finally:(fun () -> close_out chan)]
    is that if [f ()] succeeds and [close_out chan] raises an exception,
    this exception is not intercepted, and if [f ()] raises an exception,
    then [close_out_noerr] is used so that the exception raised by [f ()]
    is not masked.
*)

val add_channel_to_the_end :
  ?chunk_size:int -> Buffer.t -> in_channel -> unit
(** [add_channel_to_the_end ~chunk_size buf chan] reads [chan] up to
    reaching end of file and adds the contents read to [buf].
    [chan] is read by blocks of [chunk_size] bytes, [1024] by default.
*)

val output_channel_to_the_end :
  ?chunk_size:int -> out_channel -> in_channel -> unit
(** [output_channel_to_the_end ~chunk_size out_chan in_chan] reads [in_chan] up
    to reaching end of file and outputs the contents read to [out_chan].
    [chan] is read by blocks of [chunk_size] bytes, [1024] by default.
*)

val with_temp_file :
  ?prefix:string ->
  ?suffix:string -> string -> (string -> in_channel -> 'a) -> 'a
(** [with_temp_file ~prefix ~suffix contents f] creates a temporary file
    with a fresh [filename] forged from [prefix] and [suffix]
    (by calling [Filename.open_temp_file]), outputs [contents] in it, opens this
    file for reading as [channel], and evaluates [f filename channel].
    Once [f] returns or raises an exception, [channel] is closed and the file
    is removed.
    If [f] succeeds, the returned value is the value computed by [f].
    If [f] raises an exception, this exception is raised again.
 *)

val with_pipe : (in_channel -> out_channel -> 'a) -> 'a
(** [with_pipe f] creates a Unix pipe [(in_chan, out_chan)] and evaluates
    [f in_chan out_chan]. Channels are closed even if [f] raises an exception.
 *)

val with_stdin_from : in_channel -> (unit -> 'a) -> 'a
(** [with_stdin_from chan f] evaluates [f ()] by redirecting chan to
    the standard input. The former standard input is restored afterwards, even
    if [f] raises an exception.
 *)

val with_stdout_to : out_channel -> (unit -> 'a) -> 'a
(** [with_stdout_from chan f] evaluates [f ()] by redirecting the standard
    output to chan. The former standard output is restored afterwards, even
    if [f] raises an exception.
 *)

val with_channel_from_string : string -> (in_channel -> 'a) -> 'a
(** [with_channel_from_string s f] evaluates [f chan] where [chan] is a channel
    from which [s] can be read. *)

val with_channel_to_buffer :
  ?chunk_size:int -> Buffer.t -> (out_channel -> 'a) -> 'a
(** [with_channel_to_buffer buf f] evaluates [f chan] where [chan] is a channel,
    such that the contents written to it is added to [buf]. *)

val with_channel_to_string :
  ?initial_size:int ->
  ?chunk_size:int -> (out_channel -> 'a) -> string * 'a
(** [with_channel_to_string f] evaluates [f chan] where [chan] is a channel,
    and returns [(s, v)] where [s] is the contents written to [chan] and [v]
    is the value computed by [f]. *)

val with_stdin_from_string : string -> (unit -> 'a) -> 'a
(** [with_stdin_from_string s f] evaluates [f ()] by redirecting the standard
    input such that [s] can be read from it.
 *)

val with_stdout_to_buffer :
  ?chunk_size:int -> Buffer.t -> (unit -> 'a) -> 'a
(** [with_stdout_to_buffer buf f] evaluates [f ()] by redirecting the standard
    output such that the contents written to it is added to [buf].
 *)

val with_stdout_to_string :
  ?initial_size:int -> ?chunk_size:int -> (unit -> 'a) -> string * 'a
(** [with_channel_to_string f] evaluates [f ()] by redirecting the standard
    output and returns [(s, v)] where [s] is the contents written to the
    standard output and [v] is the value computed by [f]. *)
