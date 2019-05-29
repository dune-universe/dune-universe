
(* open/close binary channels, with LZ4-compression happening in the background,
   using a separate process and a named FIFO/pipe *)

module Ht = Hashtbl

(* filename sanitization to avoid arbitrary command execution *)
module Fn: sig
  type t
  val sanitize: string -> t
  val to_string: t -> string
end = struct

  type t = string

  let valid_unix_filename_regexp =
    (* only very conservative filenames are accepted *)
    Str.regexp "^[a-zA-Z0-9._/-]+$"

  exception Invalid_filename of string

  let sanitize (fn: string): t =
    if Str.string_match valid_unix_filename_regexp fn 0 then
      fn
    else
      raise (Invalid_filename fn)

  let to_string (fn: t): string =
    fn
end

(* register of created FIFO filenames *)
let out_chan_to_fn = Ht.create 11

let run_command ?(debug = false) (cmd: string): unit =
  if debug then Log.info "run_command: %s" cmd;
  match Unix.system cmd with
  | Unix.WSIGNALED _ -> (Log.fatal "run_command: signaled: %s" cmd; exit 1)
  | Unix.WSTOPPED _ -> (Log.fatal "run_command: stopped: %s" cmd; exit 1)
  | Unix.WEXITED i when i <> 0 ->
    (Log.fatal "run_command: exit %d: %s" i cmd; exit 1)
  | Unix.WEXITED _ (* i = 0 then *) -> ()

let lz4_open_out_bin (fn': Fn.t) =
  let fn = Fn.to_string fn' in
  let fifo_fn = fn ^ ".out.fifo" in
  Unix.mkfifo fifo_fn 0o600;
  (* launch background compression process reading from the FIFO *)
  run_command (Printf.sprintf "lz4 -f -z -1 -q %s %s &" fifo_fn fn);
  let out_chan = Pervasives.open_out_bin fifo_fn in
  assert(not (Ht.mem out_chan_to_fn out_chan));
  Ht.add out_chan_to_fn out_chan fifo_fn;
  out_chan

let lz4_close_out out_chan =
  let fifo_fn = Ht.find out_chan_to_fn out_chan in
  Ht.remove out_chan_to_fn out_chan;
  Pervasives.close_out out_chan;
  Sys.remove fifo_fn

let lz4_with_out_file (fn: Fn.t) f =
  let output = lz4_open_out_bin fn in
  let res = f output in
  lz4_close_out output;
  res

let in_chan_to_fn = Ht.create 11

(* WARNING: this works but is kind of slow.
 * Perhaps, making the FIFO size bigger (via a fcntl call) would work faster *)
let lz4_open_in_bin (fn': Fn.t) =
  let fn = Fn.to_string fn' in
  let fifo_fn = fn ^ ".in.fifo" in
  Unix.mkfifo fifo_fn 0o600;
  (* launch decompression process *)
  run_command (Printf.sprintf "lz4 -d -q %s > %s &" fn fifo_fn);
  let in_chan = Pervasives.open_in_bin fifo_fn in
  assert(not (Ht.mem in_chan_to_fn in_chan));
  Ht.add in_chan_to_fn in_chan fifo_fn;
  in_chan

let lz4_close_in in_chan =
  let fifo_fn = Ht.find in_chan_to_fn in_chan in
  Ht.remove in_chan_to_fn in_chan;
  Pervasives.close_in in_chan;
  Sys.remove fifo_fn

let lz4_with_in_file (fn: Fn.t) f =
  let input = lz4_open_in_bin fn in
  let res = f input in
  lz4_close_in input;
  res

(* aliases, for conveniency *)
let open_out_bin (fn: string) =
  lz4_open_out_bin (Fn.sanitize fn)

let close_out = lz4_close_out

let with_out_file (fn: string) f =
  lz4_with_out_file (Fn.sanitize fn) f

let open_in_bin (fn: string) =
  lz4_open_in_bin (Fn.sanitize fn)

let close_in = lz4_close_in

let with_in_file (fn: string) f =
  lz4_with_in_file (Fn.sanitize fn) f
